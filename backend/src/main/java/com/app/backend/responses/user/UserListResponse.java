package com.app.backend.responses.user;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@AllArgsConstructor
@Data
@Builder
@NoArgsConstructor
public class UserListResponse {
    @JsonProperty("users")
    private List<UserResponse> users;
    @JsonProperty("total_pages")
    private int totalPages;
}
