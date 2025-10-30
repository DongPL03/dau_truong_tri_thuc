package com.app.backend.responses.practice;

import com.app.backend.models.TheGhiNho;
import lombok.*;

import java.time.LocalDateTime;

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
    private LocalDateTime taoLuc;

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

