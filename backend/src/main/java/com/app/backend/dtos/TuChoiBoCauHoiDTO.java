package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class TuChoiBoCauHoiDTO {
    @JsonProperty("ly_do_tu_choi")
    private String lyDoTuChoi;


}
