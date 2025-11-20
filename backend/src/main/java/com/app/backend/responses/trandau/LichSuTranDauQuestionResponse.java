package com.app.backend.responses.trandau;

import com.app.backend.models.CauHoi;
import com.app.backend.models.TraLoiTranDau;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class LichSuTranDauQuestionResponse {

    @JsonProperty("cau_hoi_id")
    private Long cauHoiId;

    @JsonProperty("noi_dung")
    private String noiDung;

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
    private String dapAnDung; // 'A'/'B'/'C'/'D'

    @JsonProperty("giai_thich")
    private String giaiThich; // nếu bảng bạn có cột này

    @JsonProperty("nguoi_dung_chon")
    private String nguoiDungChon; // 'A'/'B'/'C'/'D'

    @JsonProperty("dung_hay_sai")
    private Boolean dungHaySai;

    @JsonProperty("thoi_gian_ms")
    private Long thoiGianMs;

    public static LichSuTranDauQuestionResponse fromEntities(TraLoiTranDau tl, CauHoi q) {
        return LichSuTranDauQuestionResponse.builder()
                .cauHoiId(q.getId())
                .noiDung(q.getNoiDung())
                .loaiNoiDung(q.getLoaiNoiDung())
                .duongDanTep(q.getDuongDanTep())
                .luaChonA(q.getLuaChonA())
                .luaChonB(q.getLuaChonB())
                .luaChonC(q.getLuaChonC())
                .luaChonD(q.getLuaChonD())
                .dapAnDung(String.valueOf(q.getDapAnDung()))
                .giaiThich(q.getGiaiThich()) // nếu không có thì xoá dòng này
                .nguoiDungChon(String.valueOf(tl.getLuaChon()))
                .dungHaySai(tl.getDungHaySai())
                .thoiGianMs(Long.valueOf(tl.getThoiGianMs()))
                .build();
    }
}

