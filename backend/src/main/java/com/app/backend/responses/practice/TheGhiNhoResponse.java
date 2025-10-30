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
    @JsonProperty("phien_id")
    private Long phienId;

    @JsonProperty("bo_cau_hoi")
    private String boCauHoi;

    @JsonProperty("cau_hoi")
    private String cauHoi;

    @JsonProperty("dap_an_dung")
    private Character dapAnDung;

    @JsonProperty("giai_thich")
    private String giaiThich;

    @JsonProperty("tao_luc")
    private LocalDateTime taoLuc;

    public static TheGhiNhoResponse from(TheGhiNho entity) {
        return TheGhiNhoResponse.builder()
                .phienId(entity.getPhien().getId())
                .boCauHoi(entity.getPhien().getBoCauHoi().getTieuDe())
                .cauHoi(entity.getCauHoi().getNoiDung())
                .dapAnDung(entity.getCauHoi().getDapAnDung())
                .giaiThich(entity.getCauHoi().getGiaiThich())
                .taoLuc(entity.getTaoLuc())
                .build();
    }
}
