package com.app.backend.responses.banbe;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Response gợi ý kết bạn
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class FriendSuggestionResponse {

    @JsonProperty("user_id")
    private Long userId;

    @JsonProperty("ho_ten")
    private String hoTen;

    @JsonProperty("avatar_url")
    private String avatarUrl;

    @JsonProperty("level")
    private Integer level;

    @JsonProperty("tong_diem")
    private Long tongDiem;

    /**
     * Lý do gợi ý: "SAME_BATTLE", "MUTUAL_FRIEND", "POPULAR"
     */
    @JsonProperty("reason")
    private String reason;

    /**
     * Số bạn chung (nếu có)
     */
    @JsonProperty("mutual_friends_count")
    private Integer mutualFriendsCount;
}
