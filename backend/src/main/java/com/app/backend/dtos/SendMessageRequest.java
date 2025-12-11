package com.app.backend.dtos;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

/**
 * DTO gửi tin nhắn 1-1
 * JSON sẽ là: { "receiver_id": 2, "noi_dung": "hello" }
 */
public record SendMessageRequest(
        @NotNull(message = "receiver_id không được để trống")
        Long receiver_id,

        @NotNull(message = "noi_dung không được để trống")
        @Size(min = 1, max = 1000, message = "Độ dài nội dung từ 1-1000 ký tự")
        String noi_dung
) {
}
