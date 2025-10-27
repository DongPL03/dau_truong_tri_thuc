package com.app.backend.responses.cauhoi;

import com.app.backend.models.CauHoi;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CauHoiResponse {
    private Long id;

    @JsonProperty("noi_dung")
    private String noiDung;

    @JsonProperty("do_kho")
    private String doKho;

    @JsonProperty("loai_noi_dung")
    private String loaiNoiDung;

    @JsonProperty("duong_dan_tep")
    private String duongDanTep;

    @JsonProperty("lua_chon_a")
    private String luaChonA;

    @JsonProperty("lua_chon_b")
    private String luaChonB;

    @JsonProperty("lua_chon_c")
    private String luaChonC;

    @JsonProperty("lua_chon_d")
    private String luaChonD;

    @JsonProperty("dap_an_dung")
    private Character dapAnDung;

    @JsonProperty("giai_thich")
    private String giaiThich;

    @JsonProperty("bo_cau_hoi_id")
    private Long boCauHoiId;

    @JsonProperty("bo_cau_hoi_tieu_de")
    private String boCauHoiTieuDe;

    public static CauHoiResponse from(CauHoi entity) {
        return CauHoiResponse.builder()
                .id(entity.getId())
                .noiDung(entity.getNoiDung())
                .doKho(entity.getDoKho())
                .loaiNoiDung(entity.getLoaiNoiDung())
                .duongDanTep(entity.getDuongDanTep())
                .luaChonA(entity.getLuaChonA())
                .luaChonB(entity.getLuaChonB())
                .luaChonC(entity.getLuaChonC())
                .luaChonD(entity.getLuaChonD())
                .dapAnDung(entity.getDapAnDung())
                .giaiThich(entity.getGiaiThich())
                .boCauHoiId(entity.getBoCauHoi() != null ? entity.getBoCauHoi().getId() : null)
                .boCauHoiTieuDe(entity.getBoCauHoi() != null ? entity.getBoCauHoi().getTieuDe() : null)
                .build();
    }
}
