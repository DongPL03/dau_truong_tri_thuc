package com.app.backend.responses.trandau;


import com.app.backend.models.NguoiDung;
import com.app.backend.models.TranDau;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
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

    private Long tranDauId;
    private String tenPhong;
    private String maPhong;
    private LocalDateTime batDauLuc;
    private LocalDateTime ketThucLuc;
    private int tongNguoiChoi;
    private Long winnerId;
    private String winnerTen;
    private List<PlayerScore> bangXepHang;

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class PlayerScore {
        private Long userId;
        private String hoTen;
        private int diem;
        private int thuHang;
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
                                            List<NguoiDung> allUsers) {

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
                            .thuHang(rankCounter.getAndIncrement())
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
                .build();
    }
}

