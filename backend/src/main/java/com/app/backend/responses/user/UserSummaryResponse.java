package com.app.backend.responses.user;


import com.app.backend.models.BangXepHang;
import com.app.backend.models.NguoiDung;
import com.app.backend.models.constant.RankTier;
import com.app.backend.responses.lichsutrandau.LichSuTranDauResponse;
import com.app.backend.services.bangxephang.BangXepHangService;
import com.app.backend.utils.LevelInfo;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.util.List;

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


    @JsonProperty("xep_hang")
    private Integer xepHang;

    @JsonProperty("tong_xp")
    private Long tongXp;

    @JsonProperty("level")
    private Integer level;

    @JsonProperty("rank_tier")
    private RankTier rankTier;

    @JsonProperty("xp_in_current_level")
    private Long xpInCurrentLevel;

    @JsonProperty("xp_next_level")
    private Long xpNextLevel;

    @JsonProperty("level_progress_percent")
    private Double levelProgressPercent;

    @JsonProperty("tien_vang")
    private Long tienVang;


    @JsonProperty("lich_su_tran_dau")
    private List<LichSuTranDauResponse> lichSuTranDau;

    public static UserSummaryResponse from(BangXepHang bxh,
                                           NguoiDung user,
                                           List<LichSuTranDauResponse> history,
                                           BangXepHangService bangXepHangService) {

        int tongDiem = bxh.getTongDiem() != null ? bxh.getTongDiem() : 0;
        long totalXp = bxh.getTongXp() != null ? bxh.getTongXp() : 0L;
        long tienVang = bxh.getTienVang() != null ? bxh.getTienVang() : 0L;

        LevelInfo li = bangXepHangService.computeLevelInfo(totalXp);
        RankTier tier = bangXepHangService.getRankTier(bxh);

        return UserSummaryResponse.builder()
                .userId(user.getId())
                .hoTen(user.getHoTen())
                .avatarUrl(user.getAvatarUrl())
                .tongDiem(tongDiem)
                .tongTran(bxh.getTongTran())
                .soTranThang(bxh.getSoTranThang())
                .soTranThua(bxh.getSoTranThua())
                .xepHang(bxh.getXepHang())
//                .tiLeThang(bxh.getTiLeThang())
                .tongXp(totalXp)
                .level(li.getLevel())
                .rankTier(tier)
                .xpInCurrentLevel(li.getXpInLevel())
                .xpNextLevel(li.getXpToNext())
                .levelProgressPercent(li.getProgressPercent())
                .tienVang(tienVang)
                .lichSuTranDau(history)
                .build();
    }
}
