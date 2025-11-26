package com.app.backend.responses.luyentap;

import com.app.backend.models.PhienLuyenTap;
import com.app.backend.models.TraLoiLuyenTap;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.math.BigDecimal;
import java.time.Instant;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class KetQuaLuyenTapResponse {
    @JsonProperty("phien_id")
    private Long phienId;

    @JsonProperty("bo_cau_hoi")
    private String boCauHoi;

    @JsonProperty("tong_cau_hoi")
    private Integer tongCauHoi;

    @JsonProperty("so_cau_dung")
    private Integer soCauDung;

    @JsonProperty("diem_so")
    private Integer diemSo;

    @JsonProperty("do_chinh_xac")
    private BigDecimal doChinhXac;

    @JsonProperty("thoi_gian_tb_ms")
    private Integer thoiGianTbMs;

    @JsonProperty("tao_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant taoLuc;

    @JsonProperty("chi_tiet")
    private List<AnswerDetail> chiTiet;

    @Getter
    @Setter
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class AnswerDetail {
        @JsonProperty("cau_hoi_id")
        private Long cauHoiId;
        @JsonProperty("noi_dung")
        private String noiDung;
        @JsonProperty("loai_noi_dung")
        private String loaiNoiDung;
        @JsonProperty("duong_dan_tep")
        private String duongDanTep;
        @JsonProperty("dap_an_dung")
        private Character dapAnDung;
        @JsonProperty("lua_chon")
        private Character luaChon;
        @JsonProperty("dung_hay_sai")
        private Boolean dungHaySai;
        @JsonProperty("thoi_gian_ms")
        private Integer thoiGianMs;
    }

    public static KetQuaLuyenTapResponse from(PhienLuyenTap phien, List<TraLoiLuyenTap> traLois) {
        return KetQuaLuyenTapResponse.builder()
                .phienId(phien.getId())
                .boCauHoi(phien.getBoCauHoi().getTieuDe())
                .tongCauHoi(phien.getTongCauHoi())
                .soCauDung(phien.getSoCauDung())
                .diemSo(phien.getDiemSo())
                .doChinhXac(phien.getDoChinhXac())
                .thoiGianTbMs(phien.getThoiGianTbMs())
                .taoLuc(phien.getTaoLuc())
                .chiTiet(traLois.stream().map(t ->
                        AnswerDetail.builder()
                                .cauHoiId(t.getCauHoi().getId())
                                .noiDung(t.getCauHoi().getNoiDung())
                                .loaiNoiDung(t.getCauHoi().getLoaiNoiDung())
                                .duongDanTep(t.getCauHoi().getDuongDanTep())
                                .dapAnDung(t.getCauHoi().getDapAnDung())
                                .luaChon(t.getLuaChon())
                                .dungHaySai(t.getDungHaySai())
                                .thoiGianMs(t.getThoiGianMs())
                                .build()
                ).collect(Collectors.toList()))
                .build();
    }

}
