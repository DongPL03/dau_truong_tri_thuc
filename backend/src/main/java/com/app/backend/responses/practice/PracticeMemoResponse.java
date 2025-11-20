package com.app.backend.responses.practice;

import com.app.backend.models.TheGhiNho;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.*;

import java.time.Instant;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PracticeMemoResponse {

    private Long memoId;

    private Long phienId;

    private Long cauHoiId;

    private String noiDung;

    private String luaChonA;

    private String luaChonB;

    private String luaChonC;

    private String luaChonD;

    private Character dapAnDung;

    private String boCauHoi;

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

