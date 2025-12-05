package com.app.backend.responses.notification;

import com.app.backend.models.ThongBao;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.Instant;

@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
public class NotificationResponse {
    @JsonProperty("thong_bao_id")
    private Long thongBaoId;
    @JsonProperty("nguoi_gui_id")
    private Long nguoiGuiId;
    @JsonProperty("nguoi_gui_ten")
    private String nguoiGuiTen;
    @JsonProperty("nguoi_gui_avatar_url")
    private String nguoiGuiAvatarUrl;
    @JsonProperty("loai")
    private String loai;          // FRIEND_REQUEST / BATTLE_INVITE / SYSTEM
    @JsonProperty("noi_dung")
    private String noiDung;
    @JsonProperty("metadata")
    private String metadata;      // JSON string
    @JsonProperty("da_doc")
    private Boolean daDoc;
    @JsonProperty("tao_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant taoLuc;

    public static NotificationResponse fromEntity(ThongBao t) {
        return NotificationResponse.builder()
                .thongBaoId(t.getId())
                .nguoiGuiId(t.getNguoiGui().getId())
                .nguoiGuiTen(t.getNguoiGui().getHoTen())
                .nguoiGuiAvatarUrl(t.getNguoiGui().getAvatarUrl())
                .loai(t.getLoai() != null ? t.getLoai() : null)
                .noiDung(t.getNoiDung())
                .metadata(t.getMetadata())
                .daDoc(Boolean.TRUE.equals(t.getDaDoc()))
                .taoLuc(t.getTaoLuc())
                .build();
    }
}
