package com.app.backend.responses.chat;

import com.app.backend.models.PhongChat;
import com.app.backend.models.ThanhVienPhongChat;
import com.app.backend.models.enums.LoaiPhongChat;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;
import java.util.List;
import java.util.stream.Collectors;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PhongChatResponse {

    private Long id;
    private String ten;
    private String anhNhom;
    private LoaiPhongChat loai;
    private Instant taoLuc;
    private Instant capNhatLuc;
    private String tinNhanCuoi;
    private Instant thoiGianTinNhanCuoi;
    private List<ThanhVienResponse> thanhVien;
    private long soTinNhanChuaDoc;
    private Boolean daGhim;
    private Boolean daTatThongBao;

    // Thông tin cho chat 1-1 (để hiển thị tên và avatar của người kia)
    private NguoiChatResponse nguoiChat;

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ThanhVienResponse {
        private Long id;
        private Long nguoiDungId;
        private String ten;
        private String anhDaiDien;
        private String bietDanh;
        private String vaiTro;
        private Instant thamGiaLuc;
        private Boolean online;
    }

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class NguoiChatResponse {
        private Long id;
        private String ten;
        private String anhDaiDien;
        private Boolean online;
    }

    public static PhongChatResponse fromEntity(PhongChat entity, Long currentUserId, long unreadCount) {
        if (entity == null) return null;

        PhongChatResponseBuilder builder = PhongChatResponse.builder()
                .id(entity.getId())
                .ten(entity.getTen())
                .anhNhom(entity.getAnhNhom())
                .loai(entity.getLoai())
                .taoLuc(entity.getTaoLuc())
                .capNhatLuc(entity.getCapNhatLuc())
                .tinNhanCuoi(entity.getTinNhanCuoi())
                .thoiGianTinNhanCuoi(entity.getThoiGianTinNhanCuoi())
                .soTinNhanChuaDoc(unreadCount);

        // Build danh sách thành viên
        if (entity.getThanhVien() != null) {
            List<ThanhVienResponse> thanhVienList = entity.getThanhVien().stream()
                    .filter(tv -> !tv.getDaRoi())
                    .map(tv -> ThanhVienResponse.builder()
                            .id(tv.getId())
                            .nguoiDungId(tv.getNguoiDung().getId())
                            .ten(tv.getNguoiDung().getHoTen())
                            .anhDaiDien(tv.getNguoiDung().getAvatarUrl())
                            .bietDanh(tv.getBietDanh())
                            .vaiTro(tv.getVaiTro().name())
                            .thamGiaLuc(tv.getThamGiaLuc())
                            .online(false) // Sẽ được update từ WebSocket
                            .build())
                    .collect(Collectors.toList());
            builder.thanhVien(thanhVienList);

            // Tìm thông tin cho current user
            entity.getThanhVien().stream()
                    .filter(tv -> tv.getNguoiDung().getId().equals(currentUserId) && !tv.getDaRoi())
                    .findFirst()
                    .ifPresent(tv -> {
                        builder.daGhim(tv.getDaGhim());
                        builder.daTatThongBao(tv.getDaTatThongBao());
                    });

            // Nếu là chat 1-1, lấy thông tin người kia
            if (entity.getLoai() == LoaiPhongChat.DON) {
                entity.getThanhVien().stream()
                        .filter(tv -> !tv.getNguoiDung().getId().equals(currentUserId) && !tv.getDaRoi())
                        .findFirst()
                        .ifPresent(tv -> {
                            builder.nguoiChat(NguoiChatResponse.builder()
                                    .id(tv.getNguoiDung().getId())
                                    .ten(tv.getNguoiDung().getHoTen())
                                    .anhDaiDien(tv.getNguoiDung().getAvatarUrl())
                                    .online(false)
                                    .build());
                        });
            }
        }

        return builder.build();
    }

    public static PhongChatResponse fromEntitySimple(PhongChat entity, Long currentUserId) {
        return fromEntity(entity, currentUserId, 0);
    }
}
