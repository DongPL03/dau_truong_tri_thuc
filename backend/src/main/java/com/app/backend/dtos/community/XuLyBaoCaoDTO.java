package com.app.backend.dtos.community;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.Size;

public record XuLyBaoCaoDTO(
        @JsonProperty("trang_thai")
        String trangThai, // RESOLVED, DISMISSED

        @Size(max = 500, message = "Ghi chú tối đa 500 ký tự")
        @JsonProperty("ghi_chu")
        String ghiChu,

        @JsonProperty("an_noi_dung")
        Boolean anNoiDung // Có ẩn bài viết/bình luận không
) {
}
