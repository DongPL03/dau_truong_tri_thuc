// LeaderboardUpdateEvent.java
package com.app.backend.responses.websocket;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class LeaderboardUpdateEvent {
    @JsonProperty("type")
    private String type;   // "LEADERBOARD_UPDATE"
    @JsonProperty("tran_dau_id")
    private Long tranDauId;
    @JsonProperty("players")
    private List<Row> players;  // danh sách toàn bộ người chơi xếp hạng

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Row {
        @JsonProperty("user_id")
        private Long userId;
        @JsonProperty("ho_ten")
        private String hoTen;
        @JsonProperty("diem")
        private Integer diem;
        @JsonProperty("so_cau_dung")
        private Integer soCauDung;
        @JsonProperty("xep_hang")
        private Integer xepHang;
    }
}
