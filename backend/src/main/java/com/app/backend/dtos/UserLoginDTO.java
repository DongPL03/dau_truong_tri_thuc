package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import lombok.*;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
public class UserLoginDTO {
    @JsonProperty("ten_dang_nhap")
    private String tenDangNhap;

    @JsonProperty("email")
    private String email;

    // Password may not be needed for social login but required for traditional login
    @NotBlank(message = "Password cannot be blank")
    private String password;

    @Min(value = 1, message = "You must enter role's Id")
    @JsonProperty("role_id")
    private Long roleId;

}
