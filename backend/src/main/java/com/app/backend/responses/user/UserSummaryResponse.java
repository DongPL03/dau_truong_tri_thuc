package com.app.backend.responses.user;


import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UserSummaryResponse {

    @JsonProperty("user_id")
    private Long userId;

    @JsonProperty("ho_ten")
    private String hoTen;

    @JsonProperty("avatar_url")
    private String avatarUrl;

    @JsonProperty("tong_diem")
    private Integer tongDiem;

    @JsonProperty("tong_tran")
    private Integer tongTran;

    @JsonProperty("so_tran_thang")
    private Integer soTranThang;

    @JsonProperty("so_tran_thua")
    private Integer soTranThua;

    @JsonProperty("ti_le_thang")
    private Double tiLeThang;

    @JsonProperty("rank_tier")
    private String rankTier;
}
