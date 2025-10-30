package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class SubmitAnswerDTO {

    @NotNull
    @JsonProperty("tran_dau_id")
    private Long tranDauId;

    @NotNull
    @JsonProperty("cau_hoi_id")
    private Long cauHoiId;   // câu hiện tại

    @NotBlank
    @JsonProperty("answer")
    private String answer;   // A|B|C|D (case-insensitive)
}
