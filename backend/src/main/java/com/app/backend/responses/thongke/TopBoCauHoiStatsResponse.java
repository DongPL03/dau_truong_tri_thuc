package com.app.backend.responses.thongke;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TopBoCauHoiStatsResponse {
    @JsonProperty("bo_cau_hoi_id")
    private Long boCauHoiId;
    @JsonProperty("tieu_de")
    private String tieuDe;
    @JsonProperty("so_tran")
    private long soTran;   // số trận đã dùng bộ này
}
