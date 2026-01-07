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

float rand(float2 p, float time) {
    float t = floor(time * 50.0) / 10.0;
    return fract(sin(dot(p, float2(t * 12.9898, t * 78.233))) * 43758.5453);
}

float noise(float2 uv, float blockiness, float time) {
    float2 lv = fract(uv);
    float2 id = floor(uv);
    float n1 = rand(id, time);
    float n2 = rand(id + float2(1.0, 0.0), time);
    float n3 = rand(id + float2(0.0, 1.0), time);
    float n4 = rand(id + float2(1.0, 1.0), time);
    float2 u = smoothstep(0.0, 1.0 + blockiness, lv);
    return mix(mix(n1, n2, u.x), mix(n3, n4, u.x), u.y);
}

float fbm(float2 uv, int count, float blockiness, float complexity, float time) {
    float val = 0.0;
    float amp = 0.5;
    while(count != 0) {
        val += amp * noise(uv, blockiness, time);
        amp *= 0.5;
        uv *= complexity;
        count--;
    }
    return val;
}

fragment float4 glitch_effect(VertexOut in [[stage_in]],
                             constant float2& res [[buffer(0)]],
                             constant float& time [[buffer(1)]],
                             texture2d<float, access::sample> texture [[texture(1)]]) {

    constexpr sampler s(address::clamp_to_edge, filter::linear);

    float2 uv = in.texCoord;
    float2 a = float2(uv.x * (res.x / res.y), uv.y);
    float2 uv2 = float2(a.x / res.x, exp(a.y));
    float2 id = floor(uv * 20.0);

    float shift = 0.9 * pow(fbm(uv2, int(rand(id, time) * 6.0), 10000.0, 400.0, time), 10.0);

    float scanline = abs(cos(uv.y * 400.0));
    scanline = smoothstep(0.0, 2.0, scanline);
    shift = smoothstep(0.00001, 0.2, shift);

    float colR = texture.sample(s, float2(uv.x + shift, uv.y)).r * (1. - shift);
    float colG = texture.sample(s, float2(uv.x - shift, uv.y)).g * (1. - shift) + rand(id, time) * shift;
    float colB = texture.sample(s, float2(uv.x - shift, uv.y)).b * (1. - shift);

    float3 f = float3(colR, colG, colB) - (0.1 * scanline);

    return float4(f, 1.0);
}
