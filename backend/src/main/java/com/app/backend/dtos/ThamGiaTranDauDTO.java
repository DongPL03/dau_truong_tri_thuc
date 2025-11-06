package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotNull;
import lombok.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class ThamGiaTranDauDTO {
    @NotNull
    @JsonProperty("tran_dau_id")
    private Long tranDauId;

    @JsonProperty("ma_pin")
    private String maPin; // nếu phòng có mã PIN, có thể thêm tuỳ chọn
}
