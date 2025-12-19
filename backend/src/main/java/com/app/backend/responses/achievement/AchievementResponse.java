// com.app.backend.responses.achievement.AchievementResponse.java
package com.app.backend.responses.achievement;

import com.app.backend.models.NguoiDungThanhTich;
import com.app.backend.models.constant.AchievementCode;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.Instant;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AchievementResponse {

    @JsonProperty("code")
    private String code;

    @JsonProperty("title")
    private String title;

    @JsonProperty("description")
    private String description;

    @JsonProperty("mo_khoa_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant moKhoaLuc;

    public static AchievementResponse from(NguoiDungThanhTich entity) {
        AchievementCode ac = entity.getCode();
        return AchievementResponse.builder()
                .code(ac.getCode())
                .title(ac.getTitle())
                .description(ac.getDescription())
                .moKhoaLuc(entity.getMoKhoaLuc())
                .build();
    }
}
