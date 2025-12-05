package com.app.backend.responses.admin;

import lombok.*;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AdminDashboardStatsResponse {
    private long tong_nguoi_dung;
    private long tong_bo_cau_hoi;
    private long tong_bo_cau_hoi_cho_duyet;
    private long tong_tran_dau;
}
