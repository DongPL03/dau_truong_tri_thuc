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
public class RoiTranDauDTO {
    @NotNull
    @JsonProperty("tran_dau_id")
    private Long tranDauId;
}

