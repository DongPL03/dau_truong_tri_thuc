package com.app.backend.responses.trandau;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class FinishedPlayer {

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

    @JsonProperty("max_combo")
    private Integer maxCombo;
}

