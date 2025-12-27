package com.app.backend.responses.thongke;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TopPlayerStatsResponse {

    @JsonProperty("user_id")
    private Long userId;

    @JsonProperty("ho_ten")
    private String hoTen;

    @JsonProperty("ten_dang_nhap")
    private String tenDangNhap;

    @JsonProperty("avatar_url")
    private String avatarUrl;

    @JsonProperty("tong_diem")
    private long tongDiem;

    @JsonProperty("tong_tran")
    private long tongTran;

    @JsonProperty("so_tran_thang")
    private long soTranThang;

    @JsonProperty("ti_le_thang")
    private double tiLeThang;
}
