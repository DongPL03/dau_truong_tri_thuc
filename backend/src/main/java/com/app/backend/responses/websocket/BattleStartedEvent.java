package com.app.backend.responses.websocket;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BattleStartedEvent {
    @JsonProperty("type")
    private String type;       // "BATTLE_STARTED"
    @JsonProperty("tran_dau_id")

    private Long tranDauId;
    @JsonProperty("ten_phong")

    private String tenPhong;
    @JsonProperty("bat_dau_luc")

    private LocalDateTime batDauLuc;
    @JsonProperty("tong_cau_hoi")
    private int tongCauHoi;

    @JsonProperty("thoi_gian_moi_cau_giay")
    private int thoiGianMoiCauGiay;

    @JsonProperty("dem_nguoc_truoc_giay")
    private int demNguocTruocGiay; //

}

