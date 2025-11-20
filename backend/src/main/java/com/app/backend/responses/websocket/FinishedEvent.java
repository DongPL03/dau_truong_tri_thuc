package com.app.backend.responses.websocket;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

import java.time.Instant;
import java.util.List;

@Data
@Builder
public class FinishedEvent {
    @JsonProperty("type")
    private String type;       // "FINISHED"

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

    @JsonProperty("timestamp")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant timestamp;

    @JsonProperty("winner")
    private Winner winner;

    @JsonProperty("leaderboard")
    private List<Player> leaderboard;

    @Data
    @Builder
    public static class Player {
        @JsonProperty("user_id")
        private Long userId;
        @JsonProperty("ho_ten")
        private String hoTen;
        @JsonProperty("so_cau_dung")
        private int soCauDung;
        @JsonProperty("diem")
        private int diem;
        @JsonProperty("xep_hang")
        private int xepHang;
    }

    @Data
    @Builder
    public static class Winner {
        @JsonProperty("user_id")
        private Long userId;
        @JsonProperty("ho_ten")
        private String hoTen;
        @JsonProperty("diem")
        private int diem;
        @JsonProperty("so_cau_dung")
        private int soCauDung;
    }
}

