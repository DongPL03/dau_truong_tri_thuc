package com.app.backend.responses.admin;

import lombok.*;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UserNewPerDayResponse {
    private String ngay;      // yyyy-MM-dd
    private long so_luong;    // số user đăng ký trong ngày đó
}
