// First it does a "chromatic aberration" by splitting the rgb signals by a product of sin functions
// over time, then it does a glow effect in a perceptual color space
// Based on kalgynirae's Ghostty passable glow shader and NickWest's Chromatic Aberration shader demo
// Passable glow:  https://github.com/kalgynirae/dotfiles/blob/main/ghostty/glow.glsl
// "Chromatic Aberration": https://www.shadertoy.com/view/Mds3zn

#include <metal_stdlib>

using namespace metal;

struct VertexIn {
    float2 position [[attribute(0)]];
    float2 texCoord [[attribute(1)]];
};

struct VertexOut {
    float4 position [[position]];
    float2 texCoord;
};

vertex VertexOut vertex_main(uint vertexID [[vertex_id]],
                             constant VertexIn* vertices [[buffer(0)]]) {
    VertexOut out;
    out.position = float4(vertices[vertexID].position, 0.0, 1.0);
    out.texCoord = vertices[vertexID].texCoord;
    return out;
}

// sRGB linear -> nonlinear transform from https://bottosson.github.io/posts/colorwrong/
float f(float x) {
    if (x >= 0.0031308) {
        return 1.055 * pow(x, 1.0 / 2.4) - 0.055;
    } else {
        return 12.92 * x;
    }
}

float f_inv(float x) {
    if (x >= 0.04045) {
        return pow((x + 0.055) / 1.055, 2.4);
    } else {
        return x / 12.92;
    }
}

// Oklab <-> linear sRGB conversions from https://bottosson.github.io/posts/oklab/
float4 toOklab(float4 rgb) {
    float3 c = float3(f_inv(rgb.r), f_inv(rgb.g), f_inv(rgb.b));
    float l = 0.4122214708 * c.r + 0.5363325363 * c.g + 0.0514459929 * c.b;
    float m = 0.2119034982 * c.r + 0.6806995451 * c.g + 0.1073969566 * c.b;
    float s = 0.0883024619 * c.r + 0.2817188376 * c.g + 0.6299787005 * c.b;
    float l_ = pow(l, 1.0 / 3.0);
    float m_ = pow(m, 1.0 / 3.0);
    float s_ = pow(s, 1.0 / 3.0);
    return float4(
        0.2104542553 * l_ + 0.7936177850 * m_ - 0.0040720468 * s_,
        1.9779984951 * l_ - 2.4285922050 * m_ + 0.4505937099 * s_,
        0.0259040371 * l_ + 0.7827717662 * m_ - 0.8086757660 * s_,
        rgb.a
    );
}

float4 toRgb(float4 oklab) {
    float3 c = oklab.rgb;
    float l_ = c.r + 0.3963377774 * c.g + 0.2158037573 * c.b;
    float m_ = c.r - 0.1055613458 * c.g - 0.0638541728 * c.b;
    float s_ = c.r - 0.0894841775 * c.g - 1.2914855480 * c.b;
    float l = l_ * l_ * l_;
    float m = m_ * m_ * m_;
    float s = s_ * s_ * s_;
    float3 linear_srgb = float3(
         4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
        -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
        -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s
    );
    return float4(
        clamp(f(linear_srgb.r), 0.0, 1.0),
        clamp(f(linear_srgb.g), 0.0, 1.0),
        clamp(f(linear_srgb.b), 0.0, 1.0),
        oklab.a
    );
}

// Bloom samples from https://gist.github.com/qwerasd205/c3da6c610c8ffe17d6d2d3cc7068f17f
constant float3 samples[24] = {
  float3(0.1693761725038636, 0.9855514761735895, 1),
  float3(-1.333070830962943, 0.4721463328627773, 0.7071067811865475),
  float3(-0.8464394909806497, -1.51113870578065, 0.5773502691896258),
  float3(1.554155680728463, -1.2588090085709776, 0.5),
  float3(1.681364377589461, 1.4741145918052656, 0.4472135954999579),
  float3(-1.2795157692199817, 2.088741103228784, 0.4082482904638631),
  float3(-2.4575847530631187, -0.9799373355024756, 0.3779644730092272),
  float3(0.5874641440200847, -2.7667464429345077, 0.35355339059327373),
  float3(2.997715703369726, 0.11704939884745152, 0.3333333333333333),
  float3(0.41360842451688395, 3.1351121305574803, 0.31622776601683794),
  float3(-3.167149933769243, 0.9844599011770256, 0.30151134457776363),
  float3(-1.5736713846521535, -3.0860263079123245, 0.2886751345948129),
  float3(2.888202648340422, -2.1583061557896213, 0.2773500981126146),
  float3(2.7150778983300325, 2.5745586041105715, 0.2672612419124244),
  float3(-2.1504069972377464, 3.2211410627650165, 0.2581988897471611),
  float3(-3.6548858794907493, -1.6253643308191343, 0.25),
  float3(1.0130775986052671, -3.9967078676335834, 0.24253562503633297),
  float3(4.229723673607257, 0.33081361055181563, 0.23570226039551587),
  float3(0.40107790291173834, 4.340407413572593, 0.22941573387056174),
  float3(-4.319124570236028, 1.159811599693438, 0.22360679774997896),
  float3(-1.9209044802827355, -4.160543952132907, 0.2182178902359924),
  float3(3.8639122286635708, -2.6589814382925123, 0.21320071635561041),
  float3(3.3486228404946234, 3.4331800232609, 0.20851441405707477),
  float3(-2.8769733643574344, 3.9652268864187157, 0.20412414523193154)
};

float offsetFunction(float iTime) {
    float amount = 1.0;
    float periods[4] = {6.0, 16.0, 19.0, 27.0};
    for (int i = 0; i < 4; i++) {
        amount *= 1.0 + 0.5 * sin(iTime * periods[i]);
    }
    return amount * periods[3];
}

constant float DIM_CUTOFF = 0.35;
constant float BRIGHT_CUTOFF = 0.65;
constant float ABBERATION_FACTOR = 0.05;

fragment float4 glow_rgbsplit_effect(VertexOut in [[stage_in]],
                                     constant float2& resolution [[buffer(0)]],
                                     constant float& time [[buffer(1)]],
                                     texture2d<float, access::sample> sourceTexture [[texture(0)]]) {
    constexpr sampler textureSampler(mag_filter::linear, min_filter::linear, address::clamp_to_edge);
    
    float2 uv = in.texCoord;
    float amount = offsetFunction(time);
    
    // Chromatic aberration - split RGB channels
    float3 col;
    col.r = sourceTexture.sample(textureSampler, float2(uv.x - ABBERATION_FACTOR * amount / resolution.x, uv.y)).r;
    col.g = sourceTexture.sample(textureSampler, uv).g;
    col.b = sourceTexture.sample(textureSampler, float2(uv.x + ABBERATION_FACTOR * amount / resolution.x, uv.y)).b;
    
    float4 splittedColor = float4(col, 1.0);
    float4 source = toOklab(splittedColor);
    float4 dest = source;
    
    if (source.x > DIM_CUTOFF) {
        dest.x *= 1.2;
    } else {
        float2 step = float2(1.414) / resolution;
        float3 glow = float3(0.0);
        for (int i = 0; i < 24; i++) {
            float3 s = samples[i];
            float weight = s.z;
            float4 c = toOklab(sourceTexture.sample(textureSampler, uv + s.xy * step));
            if (c.x > DIM_CUTOFF) {
                glow.yz += c.yz * weight * 0.3;
                if (c.x <= BRIGHT_CUTOFF) {
                    glow.x += c.x * weight * 0.05;
                } else {
                    glow.x += c.x * weight * 0.10;
                }
            }
        }
        dest.xyz += glow.xyz;
    }
    
    return toRgb(dest);
}
