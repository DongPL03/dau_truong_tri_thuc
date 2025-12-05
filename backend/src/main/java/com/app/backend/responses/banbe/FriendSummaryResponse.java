package com.app.backend.responses.banbe;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
public class FriendSummaryResponse {
    @JsonProperty("user_id")
    private Long userId;
    @JsonProperty("ho_ten")
    private String hoTen;
    @JsonProperty("avatar_url")
    private String avatarUrl;
    @JsonProperty("trang_thai")
    private String trangThai; // ONLINE/OFFLINE
}
