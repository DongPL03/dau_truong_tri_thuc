package com.app.backend.dtos.community;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

public record TagDTO(
        @NotBlank(message = "Tên tag không được để trống")
        @Size(max = 100, message = "Tên tag tối đa 100 ký tự")
        @JsonProperty("ten")
        String ten,

        @Size(max = 100, message = "Slug tối đa 100 ký tự")
        @JsonProperty("slug")
        String slug,

        @Size(max = 500, message = "Mô tả tối đa 500 ký tự")
        @JsonProperty("mo_ta")
        String moTa,

        @Pattern(regexp = "^#([A-Fa-f0-9]{6})$", message = "Màu sắc phải là mã hex hợp lệ (vd: #FF5733)")
        @JsonProperty("mau_sac")
        String mauSac,

        @Size(max = 50, message = "Icon tối đa 50 ký tự")
        @JsonProperty("icon")
        String icon,

        @JsonProperty("thu_tu")
        Integer thuTu,

        @JsonProperty("hien_thi")
        Boolean hienThi
) {
}
