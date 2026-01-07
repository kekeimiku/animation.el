#include <AppKit/AppKit.h>
#include <Metal/Metal.h>
#include <MetalKit/MetalKit.h>
#include <QuartzCore/QuartzCore.h>
#include <dlfcn.h>
#include <emacs-module.h>

__attribute__((used, visibility("default"))) int plugin_is_GPL_compatible;

static MTKView *glowTextView = nil;

typedef struct {
  float position[2];
  float texCoord[2];
} Vertex;

static const Vertex quadVertices[] = {
    {{-1.0f, -1.0f}, {0.0f, 1.0f}},
    {{1.0f, -1.0f}, {1.0f, 1.0f}},
    {{-1.0f, 1.0f}, {0.0f, 0.0f}},
    {{1.0f, -1.0f}, {1.0f, 1.0f}},
    {{1.0f, 1.0f}, {1.0f, 0.0f}},
    {{-1.0f, 1.0f}, {0.0f, 0.0f}},
};

@interface GlowTextView : MTKView <MTKViewDelegate>
@property(nonatomic, strong) id<MTLTexture> sourceTexture;
@property(nonatomic, strong) id<MTLCommandQueue> commandQueue;
@property(nonatomic, strong) id<MTLRenderPipelineState> pipelineState;
@property(nonatomic, strong) id<MTLBuffer> vertexBuffer;
@property(nonatomic, strong) NSBitmapImageRep *bitmapRep;
@property(nonatomic) size_t captureWidth;
@property(nonatomic) size_t captureHeight;
@property(nonatomic) dispatch_queue_t captureQueue;
@property(nonatomic) BOOL isCapturing;
- (BOOL)setupMetal;
@end

@implementation GlowTextView

- (void)drawInMTKView:(MTKView *)view {
  if (!self.isCapturing) {
    self.isCapturing = YES;
    dispatch_async(self.captureQueue, ^{
      [self captureWindowContent];
      self.isCapturing = NO;
    });
  }

  if (!self.pipelineState || !self.sourceTexture) return;

  id<MTLCommandBuffer> commandBuffer = [self.commandQueue commandBuffer];
  MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;

  if (renderPassDescriptor) {
    id<MTLRenderCommandEncoder> renderEncoder = [commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
    [renderEncoder setRenderPipelineState:self.pipelineState];
    [renderEncoder setVertexBuffer:self.vertexBuffer offset:0 atIndex:0];
    [renderEncoder setFragmentTexture:self.sourceTexture atIndex:0];
    [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle vertexStart:0 vertexCount:6];
    [renderEncoder endEncoding];

    [commandBuffer presentDrawable:view.currentDrawable];
  }

  [commandBuffer commit];
}

- (void)mtkView:(MTKView *)view drawableSizeWillChange:(CGSize)size {
}

- (void)captureWindowContent {
  NSView *contentView = [NSApp mainWindow].contentView;
  if (!contentView || !self.device) return;

  CGRect bounds = contentView.bounds;
  size_t width = bounds.size.width;
  size_t height = bounds.size.height;

  if (!self.bitmapRep || self.captureWidth != width || self.captureHeight != height) {
    self.captureWidth = width;
    self.captureHeight = height;

    self.bitmapRep = [[NSBitmapImageRep alloc] initWithBitmapDataPlanes:NULL pixelsWide:width pixelsHigh:height bitsPerSample:8 samplesPerPixel:4 hasAlpha:YES isPlanar:NO colorSpaceName:NSDeviceRGBColorSpace bytesPerRow:width * 4 bitsPerPixel:32];

    MTLTextureDescriptor *desc = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatRGBA8Unorm width:width height:height mipmapped:NO];
    desc.usage = MTLTextureUsageShaderRead;
    desc.storageMode = MTLStorageModeShared;
    self.sourceTexture = [self.device newTextureWithDescriptor:desc];
  }

  if (!self.bitmapRep || !self.sourceTexture) return;

  dispatch_sync(dispatch_get_main_queue(), ^{
    BOOL wasHidden = self.hidden;
    self.hidden = YES;

    NSGraphicsContext *nsContext = [NSGraphicsContext graphicsContextWithBitmapImageRep:self.bitmapRep];
    [NSGraphicsContext saveGraphicsState];
    [NSGraphicsContext setCurrentContext:nsContext];
    [contentView.layer renderInContext:nsContext.CGContext];
    [NSGraphicsContext restoreGraphicsState];

    self.hidden = wasHidden;
  });

  [self.sourceTexture replaceRegion:MTLRegionMake2D(0, 0, width, height) mipmapLevel:0 withBytes:[self.bitmapRep bitmapData] bytesPerRow:width * 4];
}

- (BOOL)setupMetal {
  if (self.pipelineState) return YES;

  id<MTLDevice> device = self.device;
  if (!device) return NO;

  self.commandQueue = [device newCommandQueue];
  self.captureQueue = dispatch_queue_create("com.glowtext.capture", DISPATCH_QUEUE_SERIAL);
  self.isCapturing = NO;

  Dl_info info;
  if (!dladdr((void *)&plugin_is_GPL_compatible, &info)) return NO;

  NSString *libPath = [NSString stringWithUTF8String:info.dli_fname];
  NSString *metallibPath = [[libPath stringByDeletingLastPathComponent] stringByAppendingPathComponent:@"glow-text.metallib"];
  if (![[NSFileManager defaultManager] fileExistsAtPath:metallibPath]) return NO;

  id<MTLLibrary> library = [device newLibraryWithURL:[NSURL fileURLWithPath:metallibPath] error:nil];
  if (!library) return NO;

  id<MTLFunction> vertexFunction = [library newFunctionWithName:@"vertex_main"];
  id<MTLFunction> fragmentFunction = [library newFunctionWithName:@"glow_text_effect"];

  if (!fragmentFunction || !vertexFunction) return NO;

  MTLRenderPipelineDescriptor *pipelineDescriptor = [[MTLRenderPipelineDescriptor alloc] init];
  pipelineDescriptor.vertexFunction = vertexFunction;
  pipelineDescriptor.fragmentFunction = fragmentFunction;
  pipelineDescriptor.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;

  self.pipelineState = [device newRenderPipelineStateWithDescriptor:pipelineDescriptor error:nil];
  if (!self.pipelineState) return NO;

  self.vertexBuffer = [device newBufferWithBytes:quadVertices length:sizeof(quadVertices) options:MTLResourceStorageModeShared];

  return YES;
}

@end

static void removeGlowTextOverlay() {
  if (glowTextView) {
    [glowTextView removeFromSuperview];
    glowTextView = nil;
  }
}

static void createGlowTextOverlay() {
  if (glowTextView) return;

  NSView *contentView = [NSApp mainWindow].contentView;
  glowTextView = [[GlowTextView alloc] initWithFrame:contentView.bounds device:MTLCreateSystemDefaultDevice()];

  if (![(GlowTextView *)glowTextView setupMetal]) {
    glowTextView = nil;
    return;
  }

  GlowTextView *glowView = (GlowTextView *)glowTextView;
  glowView.delegate = glowView;

  glowView.enableSetNeedsDisplay = NO;
  glowView.paused = NO;
  glowView.preferredFramesPerSecond = 60;
  glowView.clearColor = MTLClearColorMake(0, 0, 0, 0);
  glowView.framebufferOnly = YES;
  glowView.layer.opaque = NO;
  glowView.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;

  [contentView addSubview:glowView positioned:NSWindowAbove relativeTo:nil];
}

static emacs_value glow_text_render_emacs(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  dispatch_async(dispatch_get_main_queue(), ^{
    createGlowTextOverlay();
  });

  return env->intern(env, "nil");
}

static emacs_value glow_text_cleanup_emacs(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  dispatch_async(dispatch_get_main_queue(), ^{
    removeGlowTextOverlay();
  });

  return env->intern(env, "nil");
}

__attribute__((used, visibility("default"))) int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);

  emacs_value defalias = env->intern(env, "defalias");

  emacs_value render_args[] = {
      env->intern(env, "glow-text-render"),
      env->make_function(env, 0, 0, glow_text_render_emacs, R"(Render text glow effect using Metal shader.)", NULL)};
  env->funcall(env, defalias, 2, render_args);

  emacs_value cleanup_args[] = {
      env->intern(env, "glow-text-cleanup"),
      env->make_function(env, 0, 0, glow_text_cleanup_emacs, R"(Stop and clean up text glow effect.)", NULL)};
  env->funcall(env, defalias, 2, cleanup_args);

  return 0;
}
