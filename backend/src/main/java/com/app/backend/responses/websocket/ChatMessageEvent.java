package com.app.backend.responses.websocket;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

import java.time.Instant;

@Data
@Builder
public class ChatMessageEvent {

    @JsonProperty("type")
    private String type; // "CHAT_MESSAGE"

    @JsonProperty("tran_dau_id")
    private Long tranDauId;

    @JsonProperty("user_id")
    private Long userId;

    @JsonProperty("ho_ten")
    private String hoTen;

    @JsonProperty("noi_dung")
    private String noiDung;

    @JsonProperty("is_system")
    private boolean system;

    @JsonProperty("timestamp")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant timestamp;
}
