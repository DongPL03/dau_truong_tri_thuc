package com.app.backend.responses.websocket;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PlayerPresenceEvent {
    @JsonProperty("type")
    private String type;          // "PLAYER_JOINED" | "PLAYER_LEFT"
    @JsonProperty("tran_dau_id")
    private Long tranDauId;
    @JsonProperty("user_id")
    private Long userId;
    @JsonProperty("ho_ten")
    private String hoTen;
    @JsonProperty("so_nguoi_hien_tai")
    private int soNguoiHienTai;   // phòng đang có bao nhiêu người
}
