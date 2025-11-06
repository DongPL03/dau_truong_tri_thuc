package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class ChuDeDTO {
    @JsonProperty("ten")
    private String ten;

    @JsonProperty("mo_ta")
    private String moTa;
}
