package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
public class UpdateUserDTO {
    @JsonProperty("ho_ten")
    private String hoTen;

    @JsonProperty("ten_hien_thi")
    private String tenHienThi;

    @JsonProperty("email")
    private String email;

    @JsonProperty("dia_chi")
    private String diaChi;

    @JsonProperty("password")
    private String password;

    @JsonProperty("retype_password")
    private String retypePassword;

}
