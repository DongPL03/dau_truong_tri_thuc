package com.app.backend.responses.luyentap;

import com.app.backend.models.PhienLuyenTap;
import com.app.backend.models.TraLoiLuyenTap;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SubmitLuyenTapResponse {
    @JsonProperty("phien_id")
    private Long phienId;

    @JsonProperty("tong_so_cau")
    private Integer tongSoCau;

    @JsonProperty("so_cau_dung")
    private Integer soCauDung;

    @JsonProperty("diem_so")
    private Integer diemSo;

    @JsonProperty("do_chinh_xac")
    private BigDecimal doChinhXac;

    @JsonProperty("thoi_gian_tb_ms")
    private Integer thoiGianTbMs;

    @JsonProperty("chi_tiet")
    private List<AnswerResult> chiTiet;

    @Getter
    @Setter
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class AnswerResult {
        @JsonProperty("cau_hoi_id")
        private Long cauHoiId;
        @JsonProperty("lua_chon")
        private Character luaChon;
        @JsonProperty("dap_an_dung")
        private Character dapAnDung;
        @JsonProperty("dung_hay_sai")
        private Boolean dungHaySai;
        @JsonProperty("giai_thich")
        private String giaiThich;
        @JsonProperty("thoi_gian_ms")
        private Integer thoiGianMs;
    }

    public static SubmitLuyenTapResponse from(PhienLuyenTap phien, List<TraLoiLuyenTap> traLois) {
        return SubmitLuyenTapResponse.builder()
                .phienId(phien.getId())
                .tongSoCau(phien.getTongCauHoi())
                .soCauDung(phien.getSoCauDung())
                .diemSo(phien.getDiemSo())
                .doChinhXac(phien.getDoChinhXac())
                .thoiGianTbMs(phien.getThoiGianTbMs())
                .chiTiet(traLois.stream().map(t ->
                        AnswerResult.builder()
                                .cauHoiId(t.getCauHoi().getId())
                                .luaChon(t.getLuaChon())
                                .dapAnDung(t.getCauHoi().getDapAnDung())
                                .giaiThich(t.getCauHoi().getGiaiThich())
                                .dungHaySai(t.getDungHaySai())
                                .thoiGianMs(t.getThoiGianMs())
                                .build()
                ).collect(Collectors.toList()))
                .build();
    }
}
