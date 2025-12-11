package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
public class UserDTO {

    @JsonProperty("ten_dang_nhap")
    private String tenDangNhap = "";

    @JsonProperty("email")
    private String email = "";

    @JsonProperty("ho_ten")
    private String hoTen = "";

    @NotBlank(message = "Password cannot be blank")
    private String password = "";

    @JsonProperty("retype_password")
    private String retypePassword = "";

    @NotNull(message = "Role ID is required")
    @JsonProperty("role_id")
    //role admin not permitted
    private Long roleId;
}
