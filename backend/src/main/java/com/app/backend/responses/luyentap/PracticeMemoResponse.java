package com.app.backend.responses.luyentap;

import com.app.backend.models.TheGhiNho;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.Instant;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PracticeMemoResponse {
    @JsonProperty("memo_id")
    private Long memoId;

    @JsonProperty("phien_id")
    private Long phienId;

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
    private Character dapAnDung;

    @JsonProperty("bo_cau_hoi")
    private String boCauHoi;

    @JsonProperty("tao_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant taoLuc;

    public static PracticeMemoResponse from(TheGhiNho memo) {
        return PracticeMemoResponse.builder()
                .memoId(memo.getId())
                .phienId(memo.getPhien().getId())
                .cauHoiId(memo.getCauHoi().getId())
                .noiDung(memo.getCauHoi().getNoiDung())
                .luaChonA(memo.getCauHoi().getLuaChonA())
                .luaChonB(memo.getCauHoi().getLuaChonB())
                .luaChonC(memo.getCauHoi().getLuaChonC())
                .luaChonD(memo.getCauHoi().getLuaChonD())
                .dapAnDung(memo.getCauHoi().getDapAnDung())
                .boCauHoi(memo.getPhien().getBoCauHoi().getTieuDe())
                .taoLuc(memo.getTaoLuc())
                .build();
    }
}

