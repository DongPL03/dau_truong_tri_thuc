package com.app.backend.dtos.community;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BaoCaoDTO {
        private Long baiVietId; // Báo cáo bài viết

        private Long binhLuanId; // Báo cáo bình luận

        @NotBlank(message = "Loại báo cáo không được để trống")
        private String loai; // SPAM, NSFW, HARASSMENT, MISINFORMATION, COPYRIGHT, OTHER

        @Size(max = 1000, message = "Chi tiết tối đa 1000 ký tự")
        private String chiTiet;
}
