#include <metal_stdlib>

using namespace metal;

struct VertexOut {
  float4 position [[position]];
  float2 texCoord;
};

constant float2 positions[6] = {
    float2(-1.0, -1.0), float2(1.0, -1.0), float2(-1.0, 1.0),
    float2(1.0, -1.0), float2(1.0, 1.0), float2(-1.0, 1.0)};

constant float2 texCoords[6] = {
    float2(0.0, 1.0), float2(1.0, 1.0), float2(0.0, 0.0),
    float2(1.0, 1.0), float2(1.0, 0.0), float2(0.0, 0.0)};

vertex VertexOut vertex_main(uint vertexID [[vertex_id]]) {
  VertexOut out;
  out.position = float4(positions[vertexID], 0.0, 1.0);
  out.texCoord = texCoords[vertexID];
  return out;
}

float hash(float n) {
  return fract(sin(n) * 43758.5453);
}

float noise(float3 x) {
  float3 p = floor(x);
  float3 f = fract(x);
  f = f * f * (3.0 - 2.0 * f);
  float n = p.x + p.y * 57.0 + 113.0 * p.z;
  float res = mix(mix(mix(hash(n + 0.0), hash(n + 1.0), f.x),
                      mix(hash(n + 57.0), hash(n + 58.0), f.x), f.y),
                  mix(mix(hash(n + 113.0), hash(n + 114.0), f.x),
                      mix(hash(n + 170.0), hash(n + 171.0), f.x), f.y),
                  f.z);
  return res;
}

fragment half4 filter_main(VertexOut in [[stage_in]], constant float &time [[buffer(0)]], texture2d<half> sourceTexture [[texture(0)]]) {
  constexpr sampler textureSampler(mag_filter::linear, min_filter::linear);

  float blurX = noise(float3(time * 10.0, 0.0, 0.0)) * 2.0 - 1.0;
  float offsetx = blurX * 0.025;

  float blurY = noise(float3(time * 10.0, 1.0, 0.0)) * 2.0 - 1.0;
  float offsety = blurY * 0.01;

  float2 ruv = in.texCoord + float2(offsetx, offsety);
  float2 guv = in.texCoord + float2(-offsetx, -offsety);
  float2 buv = in.texCoord;

  half r = sourceTexture.sample(textureSampler, ruv).r;
  half g = sourceTexture.sample(textureSampler, guv).g;
  half b = sourceTexture.sample(textureSampler, buv).b;

  half a = sourceTexture.sample(textureSampler, in.texCoord).a;

  return half4(r, g, b, a);
}
