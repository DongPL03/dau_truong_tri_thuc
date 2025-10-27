package com.app.backend.responses.practice;

import com.app.backend.models.TheGhiNho;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TheGhiNhoResponse {

    @JsonProperty("bo_cau_hoi")
    private String boCauHoi;

    @JsonProperty("cau_hoi")
    private String cauHoi;

    @JsonProperty("dap_an_dung")
    private Character dapAnDung;

    @JsonProperty("giai_thich")
    private String giaiThich;

    @JsonProperty("ngay_luu")
    private LocalDateTime ngayLuu;

    public static TheGhiNhoResponse from(TheGhiNho entity) {
        return TheGhiNhoResponse.builder()
                .boCauHoi(entity.getPhien().getBoCauHoi().getTieuDe())
                .cauHoi(entity.getCauHoi().getNoiDung())
                .dapAnDung(entity.getCauHoi().getDapAnDung())
                .giaiThich(entity.getCauHoi().getGiaiThich())
                .ngayLuu(entity.getTaoLuc())
                .build();
    }
}
