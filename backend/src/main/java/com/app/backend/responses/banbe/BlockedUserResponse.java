package com.app.backend.responses.banbe;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;

/**
 * Response thông tin người dùng bị chặn
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BlockedUserResponse {

    @JsonProperty("block_id")
    private Long blockId;

    @JsonProperty("user_id")
    private Long userId;

    @JsonProperty("ho_ten")
    private String hoTen;

    @JsonProperty("avatar_url")
    private String avatarUrl;

    @JsonProperty("ly_do")
    private String lyDo;

    @JsonProperty("chan_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant chanLuc;
}
