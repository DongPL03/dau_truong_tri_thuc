package com.app.backend.dtos.community;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import java.util.List;

public record BaiVietDTO(
        @NotBlank(message = "Tiêu đề không được để trống")
        @Size(max = 255, message = "Tiêu đề tối đa 255 ký tự")
        String tieuDe,

        @NotBlank(message = "Nội dung không được để trống")
        String noiDung,

        String loai, // THAO_LUAN, CAU_HOI, CHIA_SE, HUONG_DAN, THONG_BAO

        List<Long> tagIds,

        List<String> hinhAnhUrls
) {
}
