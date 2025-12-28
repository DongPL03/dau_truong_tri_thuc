package com.app.backend.dtos.community;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

public record BinhLuanDTO(
        @NotNull(message = "Bài viết không được để trống")
        Long baiVietId,

        @NotBlank(message = "Nội dung bình luận không được để trống")
        String noiDung,

        Long binhLuanChaId // null nếu là comment gốc, có giá trị nếu là reply
) {
}
