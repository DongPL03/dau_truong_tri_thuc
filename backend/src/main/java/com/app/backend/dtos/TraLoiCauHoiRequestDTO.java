package com.app.backend.dtos;

import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class TraLoiCauHoiRequestDTO {
    @NotNull
    private Long phienId;

    @NotNull
    private List<CauTraLoiRequest> cauTraLoiList;

    @Data
    public static class CauTraLoiRequest {
        private Long cauHoiId;
        private Character luaChon; // A, B, C, D
        private Integer thoiGianMs; // thời gian trả lời từng câu (ms)
    }
}
