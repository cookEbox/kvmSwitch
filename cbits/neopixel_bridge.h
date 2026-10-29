#ifndef KVM_NEOPIXEL_BRIDGE_H
#define KVM_NEOPIXEL_BRIDGE_H

#include <stddef.h>
#include <stdint.h>

typedef struct kvm_led_strip kvm_led_strip;

enum {
    KVM_LED_ERROR_INVALID_HANDLE = -100,
    KVM_LED_ERROR_INVALID_ARGUMENT = -101,
    KVM_LED_ERROR_FRAME_LENGTH = -102
};

int kvm_led_open(int gpio, int count, int brightness, int strip_type,
                 kvm_led_strip **out);

int kvm_led_render(kvm_led_strip *strip, const uint32_t *colours,
                   size_t count);

int kvm_led_set_pixel(kvm_led_strip *strip, size_t index, uint32_t colour);

int kvm_led_show(kvm_led_strip *strip);

int kvm_led_clear(kvm_led_strip *strip);

int kvm_led_set_brightness(kvm_led_strip *strip, int brightness);

void kvm_led_close(kvm_led_strip *strip);

const char *kvm_led_error_string(int error);

#endif
