package com.app.backend.responses.bangxephang;

import com.app.backend.models.BangXepHang;
import com.app.backend.models.constant.RankTier;
import com.app.backend.services.bangxephang.BangXepHangService;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class LeaderboardEntryResponse {

    @JsonProperty("user_id")
    private Long userId;

    @JsonProperty("ho_ten")
    private String hoTen;

    @JsonProperty("anh_dai_dien")
    private String anhDaiDien;

    @JsonProperty("tong_diem")
    private Integer tongDiem;

    @JsonProperty("tong_tran")
    private Integer tongTran;

    @JsonProperty("so_tran_thang")
    private Integer soTranThang;

    @JsonProperty("so_tran_thua")
    private Integer soTranThua;

    @JsonProperty("ti_le_thang")
    private Double tiLeThang; // % thắng

    @JsonProperty("level")
    private Integer level;

    @JsonProperty("rank_tier")
    private RankTier rankTier;

    @JsonProperty("xep_hang")
    private Integer xepHang;


    public static LeaderboardEntryResponse from(BangXepHang bxh, BangXepHangService bangXepHangService) {
        var user = bxh.getNguoiDung();
        int tongTran = bxh.getTongTran() != null ? bxh.getTongTran() : 0;
        int soThang = bxh.getSoTranThang() != null ? bxh.getSoTranThang() : 0;

        double winRate = 0.0;
        if (tongTran > 0) {
            winRate = (soThang * 100.0) / tongTran;
        }
        RankTier tier = bangXepHangService.getRankTier(bxh);
        return LeaderboardEntryResponse.builder()
                .userId(user.getId())
                .hoTen(user.getHoTen())
                .anhDaiDien(user.getAvatarUrl()) // nếu có field này
                .tongDiem(bxh.getTongDiem())
                .tongTran(tongTran)
                .soTranThang(soThang)
                .soTranThua(bxh.getSoTranThua())
                .tiLeThang(winRate)
                .level(bxh.getLevel())
                .rankTier(tier)
                .xepHang(bxh.getXepHang())
                .build();
    }
}

