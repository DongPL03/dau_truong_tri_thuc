package com.app.backend.responses.vaitro;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class VaiTroResponse {
    @JsonProperty("id")
    private Long id;

    @JsonProperty("ten_vai_tro")
    private String tenVaiTro;

    public static VaiTroResponse fromVaiTro(com.app.backend.models.VaiTro vaiTro) {
        return VaiTroResponse.builder()
                .id(vaiTro.getId())
                .tenVaiTro(vaiTro.getTenVaiTro())
                .build();
    }
}
