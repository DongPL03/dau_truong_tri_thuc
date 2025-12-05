package com.app.backend.responses.banbe;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.Instant;

@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
public class FriendRequestItemResponse {
    @JsonProperty("request_id")
    private Long requestId;
    @JsonProperty("nguoi_gui_id")
    private Long nguoiGuiId;
    @JsonProperty("nguoi_gui_ten")
    private String nguoiGuiTen;
    @JsonProperty("nguoi_nhan_id")
    private Long nguoiNhanId;
    @JsonProperty("nguoi_nhan_ten")
    private String nguoiNhanTen;
    @JsonProperty("trang_thai")
    private String trangThai;
    @JsonProperty("tao_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant taoLuc;
}

