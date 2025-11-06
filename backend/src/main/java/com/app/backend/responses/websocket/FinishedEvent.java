package com.app.backend.responses.websocket;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
public class FinishedEvent {
    private String type;        // "FINISHED"
    private Long tranDauId;
    private String tenPhong;
    private String maPhong;
    private LocalDateTime batDauLuc;
    private LocalDateTime ketThucLuc;
    private Winner winner;
    private List<Player> leaderboard;

    @Data
    @Builder
    public static class Player {
        private Long userId;
        private String hoTen;
        private int diem;
        private int thuHang;
    }

    @Data
    @Builder
    public static class Winner {
        private Long userId;
        private String hoTen;
    }
}

