package com.app.backend.responses.trandau;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Response thông tin người chơi trong phòng (trước khi trận đấu bắt đầu)
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class NguoiChoiTrongPhongResponse {

    @JsonProperty("user_id")
    private Long userId;

    @JsonProperty("ho_ten")
    private String hoTen;

    @JsonProperty("avatar_url")
    private String avatarUrl;

    @JsonProperty("la_chu_phong")
    private boolean laChuPhong;

    @JsonProperty("da_san_sang")
    private boolean daSanSang;

    @JsonProperty("tham_gia_luc")
    private String thamGiaLuc;
}
