package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
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
    @JsonProperty("phien_id")
    private Long phienId;

    @NotNull
    @JsonProperty("cau_tra_loi_list")
    private List<CauTraLoiRequest> cauTraLoiList;

    @Data
    public static class CauTraLoiRequest {
        private Long cauHoiId;
        private Character luaChon; // A, B, C, D
        private Integer thoiGianMs; // thời gian trả lời từng câu (ms)
    }
}
