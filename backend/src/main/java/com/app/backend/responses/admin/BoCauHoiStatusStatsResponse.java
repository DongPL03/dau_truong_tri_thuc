package com.app.backend.responses.admin;

import lombok.*;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BoCauHoiStatusStatsResponse {
    private long so_luong_da_duyet;
    private long so_luong_cho_duyet;
    private long so_luong_tu_choi;
}
