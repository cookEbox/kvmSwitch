#include "neopixel_bridge.h"

#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include <ws2811.h>

struct kvm_led_strip {
    ws2811_t ws2811;
    int count;
    int initialized;
};

static int valid_gpio(int gpio)
{
    return gpio == 10 || gpio == 12 || gpio == 13 || gpio == 18 ||
           gpio == 19 || gpio == 21 || gpio == 31;
}

static int valid_strip_type(int strip_type)
{
    return strip_type == WS2811_STRIP_RGB || strip_type == WS2811_STRIP_GRB;
}

static int valid_strip(const kvm_led_strip *strip)
{
    return strip != NULL && strip->initialized &&
           strip->ws2811.channel[0].leds != NULL;
}

int kvm_led_open(int gpio, int count, int brightness, int strip_type,
                 kvm_led_strip **out)
{
    kvm_led_strip *strip;
    int result;

    if (out == NULL) {
        return KVM_LED_ERROR_INVALID_ARGUMENT;
    }
    *out = NULL;

    if (!valid_gpio(gpio) || count <= 0 || count > INT_MAX ||
        brightness < 0 || brightness > 255 || !valid_strip_type(strip_type)) {
        return KVM_LED_ERROR_INVALID_ARGUMENT;
    }

    strip = calloc(1, sizeof(*strip));
    if (strip == NULL) {
        return WS2811_ERROR_OUT_OF_MEMORY;
    }

    strip->count = count;
    strip->ws2811.freq = WS2811_TARGET_FREQ;
    strip->ws2811.dmanum = 10;
    strip->ws2811.channel[0].gpionum = gpio;
    strip->ws2811.channel[0].count = count;
    strip->ws2811.channel[0].strip_type = strip_type;
    strip->ws2811.channel[0].brightness = (uint8_t)brightness;
    /* Channel 1 is deliberately left zeroed: it is unused by this adapter. */

    result = ws2811_init(&strip->ws2811);
    if (result != WS2811_SUCCESS) {
        /* The upstream initializer owns cleanup of its internal partial state. */
        free(strip);
        return result;
    }

    strip->initialized = 1;
    *out = strip;
    return WS2811_SUCCESS;
}

int kvm_led_render(kvm_led_strip *strip, const uint32_t *colours,
                   size_t count)
{
    if (!valid_strip(strip)) {
        return KVM_LED_ERROR_INVALID_HANDLE;
    }
    if (colours == NULL || count != (size_t)strip->count) {
        return count == (size_t)strip->count
            ? KVM_LED_ERROR_INVALID_ARGUMENT
            : KVM_LED_ERROR_FRAME_LENGTH;
    }

    memcpy(strip->ws2811.channel[0].leds, colours,
           count * sizeof(*colours));
    return ws2811_render(&strip->ws2811);
}

int kvm_led_set_pixel(kvm_led_strip *strip, size_t index, uint32_t colour)
{
    if (!valid_strip(strip)) {
        return KVM_LED_ERROR_INVALID_HANDLE;
    }
    if (index >= (size_t)strip->count) {
        return KVM_LED_ERROR_INVALID_ARGUMENT;
    }

    strip->ws2811.channel[0].leds[index] = colour;
    return WS2811_SUCCESS;
}

int kvm_led_show(kvm_led_strip *strip)
{
    if (!valid_strip(strip)) {
        return KVM_LED_ERROR_INVALID_HANDLE;
    }
    return ws2811_render(&strip->ws2811);
}

int kvm_led_clear(kvm_led_strip *strip)
{
    if (!valid_strip(strip)) {
        return KVM_LED_ERROR_INVALID_HANDLE;
    }

    memset(strip->ws2811.channel[0].leds, 0,
           (size_t)strip->count * sizeof(*strip->ws2811.channel[0].leds));
    return ws2811_render(&strip->ws2811);
}

int kvm_led_set_brightness(kvm_led_strip *strip, int brightness)
{
    if (!valid_strip(strip)) {
        return KVM_LED_ERROR_INVALID_HANDLE;
    }
    if (brightness < 0 || brightness > 255) {
        return KVM_LED_ERROR_INVALID_ARGUMENT;
    }

    strip->ws2811.channel[0].brightness = (uint8_t)brightness;
    return WS2811_SUCCESS;
}

void kvm_led_close(kvm_led_strip *strip)
{
    if (strip == NULL) {
        return;
    }
    if (strip->initialized) {
        ws2811_fini(&strip->ws2811);
        strip->initialized = 0;
    }
    free(strip);
}

const char *kvm_led_error_string(int error)
{
    switch (error) {
    case KVM_LED_ERROR_INVALID_HANDLE:
        return "Invalid LED strip handle";
    case KVM_LED_ERROR_INVALID_ARGUMENT:
        return "Invalid LED strip argument";
    case KVM_LED_ERROR_FRAME_LENGTH:
        return "LED frame length does not match strip length";
    default:
        return ws2811_get_return_t_str((ws2811_return_t)error);
    }
}
