package com.app.backend.responses.trandau;


import com.app.backend.models.NguoiDung;
import com.app.backend.models.TranDau;
import com.app.backend.responses.achievement.AchievementResponse;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BattleFinishResponse {
    @JsonProperty("tran_dau_id")
    private Long tranDauId;

    @JsonProperty("ten_phong")
    private String tenPhong;

    @JsonProperty("ma_phong")
    private String maPhong;

    @JsonProperty("bat_dau_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant batDauLuc;

    @JsonProperty("ket_thuc_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant ketThucLuc;

    @JsonProperty("tong_nguoi_choi")
    private int tongNguoiChoi;

    @JsonProperty("winner_id")
    private Long winnerId;

    @JsonProperty("winner_ten")
    private String winnerTen;

    @JsonProperty("bang_xep_hang")
    private List<PlayerScore> bangXepHang;

    @JsonProperty("reward")
    private MatchRewardResponse reward;

    @JsonProperty("new_achievements")
    private List<AchievementResponse> newAchievements;

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class PlayerScore {
        @JsonProperty("user_id")
        private Long userId;
        @JsonProperty("ho_ten")
        private String hoTen;
        @JsonProperty("diem")
        private int diem;
        @JsonProperty("xep_hang")
        private int xepHang;
    }

    /**
     * ⚙️ Factory method tiện lợi để tạo response từ entity + điểm.
     *
     * @param tranDau  Entity trận đấu
     * @param scores   Map userId → điểm
     * @param allUsers Danh sách người chơi (để lấy họ tên)
     */
    public static BattleFinishResponse from(TranDau tranDau,
                                            Map<Long, Integer> scores,
                                            List<NguoiDung> allUsers,
                                            MatchRewardResponse reward,
                                            List<AchievementResponse> myNewAchievements) {

        // 1️⃣ Chuẩn bị map userId → họ tên
        Map<Long, String> nameMap = (allUsers != null)
                ? allUsers.stream().collect(Collectors.toMap(NguoiDung::getId, NguoiDung::getHoTen))
                : new HashMap<>();

        // 2️⃣ Nếu có điểm, sắp xếp theo điểm giảm dần
        List<PlayerScore> rankList = new ArrayList<>();
        if (scores != null && !scores.isEmpty()) {
            AtomicInteger rankCounter = new AtomicInteger(1);
            rankList = scores.entrySet().stream()
                    .sorted(Map.Entry.<Long, Integer>comparingByValue().reversed())
                    .map(e -> PlayerScore.builder()
                            .userId(e.getKey())
                            .hoTen(nameMap.getOrDefault(e.getKey(), "Người chơi"))
                            .diem(e.getValue())
                            .xepHang(rankCounter.getAndIncrement())
                            .build())
                    .collect(Collectors.toList());
        }

        // 3️⃣ Xác định người thắng
        Long winnerId = null;
        String winnerTen = null;
        if (!rankList.isEmpty()) {
            PlayerScore top = rankList.get(0);
            winnerId = top.getUserId();
            winnerTen = top.getHoTen();
        } else if (tranDau.getWinner() != null) {
            winnerId = tranDau.getWinner().getId();
            winnerTen = tranDau.getWinner().getHoTen();
        }

        // 4️⃣ Build response
        return BattleFinishResponse.builder()
                .tranDauId(tranDau.getId())
                .tenPhong(tranDau.getTenPhong())
                .maPhong(tranDau.getMaPhong())
                .batDauLuc(tranDau.getBatDauLuc())
                .ketThucLuc(tranDau.getKetThucLuc())
                .tongNguoiChoi(rankList.size())
                .winnerId(winnerId)
                .winnerTen(winnerTen)
                .bangXepHang(rankList)
                .reward(reward)
                .newAchievements(myNewAchievements)
                .build();
    }
}

