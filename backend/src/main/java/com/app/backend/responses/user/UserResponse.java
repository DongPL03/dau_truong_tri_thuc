package com.app.backend.responses.user;

import com.app.backend.models.NguoiDung;
import com.app.backend.models.VaiTro;
import com.app.backend.responses.BaseResponse;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.Instant;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UserResponse extends BaseResponse {
    @JsonProperty("id")
    private Long id;

    @JsonProperty("ten_dang_nhap")
    private String tenDangNhap;

    @JsonProperty("email")
    private String email;

    @JsonProperty("ho_ten")
    private String hoTen;

    @JsonProperty("dia_chi")
    private String diaChi;

    @JsonProperty("ten_hien_thi")
    private String tenHienThi;

    @JsonProperty("avatar_url")
    private String avatarUrl;

    @JsonProperty("is_active")
    private boolean active;

    @JsonProperty("is_xoa")
    private boolean isXoa;

    @JsonProperty("vai_tro")
    private VaiTro vaiTro;

    @JsonProperty("tao_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant createdAt;

    public static UserResponse fromUser(NguoiDung nguoiDung) {
        return UserResponse.builder()
                .id(nguoiDung.getId())
                .tenDangNhap(nguoiDung.getTenDangNhap())
                .hoTen(nguoiDung.getHoTen())
                .diaChi(nguoiDung.getDiaChi())
                .email(nguoiDung.getEmail())
                .tenHienThi(nguoiDung.getTenHienThi())
                .avatarUrl(nguoiDung.getAvatarUrl())
                .active(nguoiDung.isActive())
                .isXoa(nguoiDung.isXoa())
                .vaiTro(nguoiDung.getVaiTro())
                .createdAt(nguoiDung.getTaoLuc())
                .build();
    }
}
