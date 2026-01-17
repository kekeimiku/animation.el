#import <AppKit/AppKit.h>
#import <Metal/Metal.h>
#import <MetalKit/MetalKit.h>
#import <QuartzCore/QuartzCore.h>
#import <emacs-module.h>

__attribute__((used, visibility("default"))) int plugin_is_GPL_compatible;

@interface MetalLoaderView : MTKView <MTKViewDelegate>
@property(nonatomic, strong) id<MTLTexture> sourceTexture;
@property(nonatomic, strong) id<MTLCommandQueue> commandQueue;
@property(nonatomic, strong) id<MTLRenderPipelineState> pipelineState;
@property(nonatomic, strong) id<MTLBuffer> timeBuffer;
@property(nonatomic, strong) NSBitmapImageRep *bitmapRep;
@property(nonatomic) CGFloat startTime;
@property(nonatomic) dispatch_queue_t captureQueue;
- (BOOL)setupMetalWithLibPath:(NSString *)libPath;
@end

@implementation MetalLoaderView

- (void)drawInMTKView:(MTKView *)view {
  dispatch_async(self.captureQueue, ^{
    [self captureWindowContent];
  });

  if (!self.pipelineState || !self.sourceTexture) return;

  float currentTime = (float)(CACurrentMediaTime() - self.startTime);
  memcpy([self.timeBuffer contents], &currentTime, sizeof(float));

  id<MTLCommandBuffer> commandBuffer = [self.commandQueue commandBuffer];
  MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;

  if (renderPassDescriptor) {
    id<MTLRenderCommandEncoder> renderEncoder = [commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
    [renderEncoder setRenderPipelineState:self.pipelineState];
    [renderEncoder setFragmentBuffer:self.timeBuffer offset:0 atIndex:0];
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
  if (width == 0 || height == 0) return;

  if (!self.bitmapRep || self.bitmapRep.pixelsWide != width || self.bitmapRep.pixelsHigh != height) {
    self.bitmapRep = [[NSBitmapImageRep alloc] initWithBitmapDataPlanes:NULL pixelsWide:width pixelsHigh:height bitsPerSample:8 samplesPerPixel:4 hasAlpha:YES isPlanar:NO colorSpaceName:NSDeviceRGBColorSpace bytesPerRow:width * 4 bitsPerPixel:32];
    MTLTextureDescriptor *desc = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatBGRA8Unorm width:width height:height mipmapped:NO];
    desc.usage = MTLTextureUsageShaderRead;
    self.sourceTexture = [self.device newTextureWithDescriptor:desc];
  }

  if (!self.bitmapRep || !self.sourceTexture) return;

  dispatch_sync(dispatch_get_main_queue(), ^{
    BOOL wasHidden = self.hidden;
    self.hidden = YES;
    NSGraphicsContext *ctx = [NSGraphicsContext graphicsContextWithBitmapImageRep:self.bitmapRep];
    [NSGraphicsContext saveGraphicsState];
    [NSGraphicsContext setCurrentContext:ctx];
    [contentView.layer renderInContext:ctx.CGContext];
    [NSGraphicsContext restoreGraphicsState];
    self.hidden = wasHidden;
  });

  [self.sourceTexture replaceRegion:MTLRegionMake2D(0, 0, width, height) mipmapLevel:0 withBytes:[self.bitmapRep bitmapData] bytesPerRow:width * 4];
}

- (BOOL)setupMetalWithLibPath:(NSString *)libPath {
  if (self.pipelineState) return YES;

  id<MTLDevice> device = self.device;
  if (!device) return NO;

  self.commandQueue = [device newCommandQueue];
  self.captureQueue = dispatch_queue_create("com.metal-loader.capture", DISPATCH_QUEUE_SERIAL);
  self.startTime = CACurrentMediaTime();

  NSError *error = nil;
  id<MTLLibrary> library = [device newLibraryWithURL:[NSURL fileURLWithPath:libPath] error:&error];
  if (!library) return NO;

  id<MTLFunction> vertexFunction = [library newFunctionWithName:@"vertex_main"];
  id<MTLFunction> fragmentFunction = [library newFunctionWithName:@"filter_main"];
  if (!fragmentFunction || !vertexFunction) return NO;

  MTLRenderPipelineDescriptor *desc = [[MTLRenderPipelineDescriptor alloc] init];
  desc.vertexFunction = vertexFunction;
  desc.fragmentFunction = fragmentFunction;
  desc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;

  self.pipelineState = [device newRenderPipelineStateWithDescriptor:desc error:&error];
  if (!self.pipelineState) return NO;

  self.timeBuffer = [device newBufferWithLength:sizeof(float) options:MTLResourceStorageModeShared];
  return YES;
}

@end

static MetalLoaderView *gMetalLoaderView = nil;

static emacs_value load_filter(emacs_env *env, ptrdiff_t n, emacs_value *args, void *data) {
  if (n < 1) return env->intern(env, "nil");

  ptrdiff_t len;
  env->copy_string_contents(env, args[0], NULL, &len);
  char *libpath = malloc(len);
  env->copy_string_contents(env, args[0], libpath, &len);

  NSString *lib = [NSString stringWithUTF8String:libpath];
  free(libpath);

  dispatch_async(dispatch_get_main_queue(), ^{
    if (gMetalLoaderView) {
      [gMetalLoaderView removeFromSuperview];
      gMetalLoaderView = nil;
    }

    NSView *contentView = [NSApp mainWindow].contentView;
    if (!contentView) return;

    id<MTLDevice> device = MTLCreateSystemDefaultDevice();
    gMetalLoaderView = [[MetalLoaderView alloc] initWithFrame:contentView.bounds device:device];

    if ([gMetalLoaderView setupMetalWithLibPath:lib]) {
      gMetalLoaderView.delegate = gMetalLoaderView;
      gMetalLoaderView.enableSetNeedsDisplay = NO;
      gMetalLoaderView.paused = NO;
      gMetalLoaderView.clearColor = MTLClearColorMake(0, 0, 0, 0);
      gMetalLoaderView.framebufferOnly = YES;
      gMetalLoaderView.layer.opaque = NO;
      gMetalLoaderView.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;
      [contentView addSubview:gMetalLoaderView positioned:NSWindowAbove relativeTo:nil];
    }
  });

  return env->intern(env, "t");
}

static emacs_value stop_filter(emacs_env *env, ptrdiff_t n, emacs_value *args, void *data) {
  dispatch_async(dispatch_get_main_queue(), ^{
    if (gMetalLoaderView) {
      [gMetalLoaderView removeFromSuperview];
      gMetalLoaderView = nil;
    }
  });
  return env->intern(env, "nil");
}

__attribute__((used, visibility("default"))) int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);

  emacs_value defalias = env->intern(env, "defalias");

  emacs_value load_args[] = {
    env->intern(env, "metal-loader-load"),
    env->make_function(env, 1, 1, load_filter, "Load and start Metal filter. Args: METALLIB-PATH", NULL)
  };
  env->funcall(env, defalias, 2, load_args);

  emacs_value stop_args[] = {
    env->intern(env, "metal-loader-stop"),
    env->make_function(env, 0, 0, stop_filter, "Stop Metal filter", NULL)
  };
  env->funcall(env, defalias, 2, stop_args);

  return 0;
}
