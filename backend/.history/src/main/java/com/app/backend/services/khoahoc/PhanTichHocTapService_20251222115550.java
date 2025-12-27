package com.app.backend.services.khoahoc;

import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.models.*;
import com.app.backend.repositories.*;
import com.app.backend.responses.khoahoc.PhanTichHocTapResponse;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class PhanTichHocTapService implements IPhanTichHocTapService {

    private final IPhanTichHocTapRepository phanTichHocTapRepository;
    private final IKhoaHocRepository khoaHocRepository;
    private final INguoiDungRepository nguoiDungRepository;
    private final IKhoaHocBoCauHoiRepository khoaHocBoCauHoiRepository;
    private final IPhienLuyenTapRepository phienLuyenTapRepository;
    private final ITraLoiLuyenTapRepository traLoiLuyenTapRepository;
    private final ObjectMapper objectMapper;

    @Override
    @Transactional
    public PhanTichHocTapResponse phanTichKhoaHoc(Long userId, Long khoaHocId) throws DataNotFoundException {
        // 1. Kiểm tra khóa học tồn tại
        KhoaHoc khoaHoc = khoaHocRepository.findById(khoaHocId)
                .orElseThrow(() -> new DataNotFoundException("Khóa học không tồn tại"));

        // 2. Lấy tất cả các bộ câu hỏi trong khóa học
        List<KhoaHocBoCauHoi> khoaHocBoCauHoiList = khoaHocBoCauHoiRepository.findByKhoaHocIdOrderByThuTuAsc(khoaHocId);
        Set<Long> boCauHoiIds = khoaHocBoCauHoiList.stream()
                .map(khbch -> khbch.getBoCauHoi().getId())
                .collect(Collectors.toSet());

        if (boCauHoiIds.isEmpty()) {
            throw new DataNotFoundException("Khóa học này chưa có bộ câu hỏi nào");
        }

        // 3. Lấy tất cả các phiên luyện tập của user trong các bộ câu hỏi này
        List<PhienLuyenTap> phienList = phienLuyenTapRepository.findByNguoiDungId(userId, 
                org.springframework.data.domain.Pageable.unpaged()).getContent();
        
        List<PhienLuyenTap> phienTrongKhoaHoc = phienList.stream()
                .filter(phien -> phien.getBoCauHoi() != null 
                        && boCauHoiIds.contains(phien.getBoCauHoi().getId()))
                .collect(Collectors.toList());

        if (phienTrongKhoaHoc.isEmpty()) {
            throw new DataNotFoundException("Bạn chưa có phiên luyện tập nào trong khóa học này");
        }

        // 4. Lấy tất cả các câu trả lời từ các phiên này
        Map<Long, List<TraLoiLuyenTap>> traLoiByPhien = new HashMap<>();
        for (PhienLuyenTap phien : phienTrongKhoaHoc) {
            List<TraLoiLuyenTap> traLois = traLoiLuyenTapRepository.findByPhienLuyenTapId(phien.getId());
            traLoiByPhien.put(phien.getId(), traLois);
        }

        // 5. Phân tích theo chủ đề
        Map<Long, ChuDeStats> chuDeStatsMap = new HashMap<>();
        
        for (Map.Entry<Long, List<TraLoiLuyenTap>> entry : traLoiByPhien.entrySet()) {
            for (TraLoiLuyenTap traLoi : entry.getValue()) {
                CauHoi cauHoi = traLoi.getCauHoi();
                if (cauHoi == null || cauHoi.getChuDe() == null) {
                    continue;
                }
                
                Long chuDeId = cauHoi.getChuDe().getId();
                ChuDeStats stats = chuDeStatsMap.computeIfAbsent(chuDeId, 
                        k -> new ChuDeStats(cauHoi.getChuDe().getTen()));
                
                stats.tongCau++;
                if (Boolean.TRUE.equals(traLoi.getDungHaySai())) {
                    stats.soCauDung++;
                }
            }
        }

        // 6. Tính tỉ lệ đúng cho mỗi chủ đề và phân loại
        List<PhanTichHocTapResponse.ChuDeInfo> chuDeManh = new ArrayList<>();
        List<PhanTichHocTapResponse.ChuDeInfo> chuDeYeu = new ArrayList<>();
        List<String> diemManh = new ArrayList<>();
        List<String> diemYeu = new ArrayList<>();

        for (Map.Entry<Long, ChuDeStats> entry : chuDeStatsMap.entrySet()) {
            ChuDeStats stats = entry.getValue();
            double tiLeDung = stats.tongCau > 0 
                    ? (double) stats.soCauDung / stats.tongCau * 100.0 
                    : 0.0;
            
            PhanTichHocTapResponse.ChuDeInfo chuDeInfo = PhanTichHocTapResponse.ChuDeInfo.builder()
                    .id(entry.getKey())
                    .ten(stats.ten)
                    .tiLeDung(tiLeDung)
                    .build();

            // Phân loại: >= 70% là mạnh, < 50% là yếu
            if (tiLeDung >= 70.0 && stats.tongCau >= 5) {
                chuDeManh.add(chuDeInfo);
                diemManh.add(stats.ten);
            } else if (tiLeDung < 50.0 && stats.tongCau >= 3) {
                chuDeYeu.add(chuDeInfo);
                diemYeu.add(stats.ten);
            }
        }

        // 7. Tạo giải pháp cải thiện
        String giaiPhap = taoGiaiPhap(chuDeYeu, chuDeManh);

        // 8. Lưu vào database
        NguoiDung nguoiDung = nguoiDungRepository.findById(userId)
                .orElseThrow(() -> new DataNotFoundException("Người dùng không tồn tại"));
        
        PhanTichHocTap phanTich = phanTichHocTapRepository
                .findByNguoiDungIdAndKhoaHocId(userId, khoaHocId)
                .orElse(PhanTichHocTap.builder()
                        .nguoiDung(nguoiDung)
                        .khoaHoc(khoaHoc)
                        .build());

        try {
            phanTich.setDiemManh(objectMapper.writeValueAsString(diemManh));
            phanTich.setDiemYeu(objectMapper.writeValueAsString(diemYeu));
            phanTich.setChuDeManh(objectMapper.writeValueAsString(
                    chuDeManh.stream().map(PhanTichHocTapResponse.ChuDeInfo::getTen).collect(Collectors.toList())));
            phanTich.setChuDeYeu(objectMapper.writeValueAsString(
                    chuDeYeu.stream().map(PhanTichHocTapResponse.ChuDeInfo::getTen).collect(Collectors.toList())));
            phanTich.setGiaiPhap(giaiPhap);
        } catch (Exception e) {
            throw new RuntimeException("Lỗi khi lưu phân tích", e);
        }

        phanTich = phanTichHocTapRepository.save(phanTich);

        // 9. Trả về response
        return PhanTichHocTapResponse.builder()
                .id(phanTich.getId())
                .khoaHocId(khoaHocId)
                .khoaHocTen(khoaHoc.getTen())
                .diemManh(diemManh)
                .diemYeu(diemYeu)
                .chuDeManh(chuDeManh)
                .chuDeYeu(chuDeYeu)
                .giaiPhap(giaiPhap)
                .capNhatLuc(phanTich.getCapNhatLuc())
                .build();
    }

    @Override
    @Transactional(readOnly = true)
    public PhanTichHocTapResponse getPhanTich(Long userId, Long khoaHocId) {
        return phanTichHocTapRepository.findByNguoiDungIdAndKhoaHocId(userId, khoaHocId)
                .map(this::mapToResponse)
                .orElse(null);
    }

    private PhanTichHocTapResponse mapToResponse(PhanTichHocTap phanTich) {
        try {
            List<String> diemManh = objectMapper.readValue(phanTich.getDiemManh(), 
                    new TypeReference<List<String>>() {});
            List<String> diemYeu = objectMapper.readValue(phanTich.getDiemYeu(), 
                    new TypeReference<List<String>>() {});
            List<String> chuDeManhTen = objectMapper.readValue(phanTich.getChuDeManh(), 
                    new TypeReference<List<String>>() {});
            List<String> chuDeYeuTen = objectMapper.readValue(phanTich.getChuDeYeu(), 
                    new TypeReference<List<String>>() {});

            // Tạo chuDeInfo từ tên (cần query lại để lấy ID và tiLeDung)
            // Tạm thời để null, có thể cải thiện sau
            List<PhanTichHocTapResponse.ChuDeInfo> chuDeManh = new ArrayList<>();
            List<PhanTichHocTapResponse.ChuDeInfo> chuDeYeu = new ArrayList<>();

            return PhanTichHocTapResponse.builder()
                    .id(phanTich.getId())
                    .khoaHocId(phanTich.getKhoaHoc().getId())
                    .khoaHocTen(phanTich.getKhoaHoc().getTen())
                    .diemManh(diemManh)
                    .diemYeu(diemYeu)
                    .chuDeManh(chuDeManh)
                    .chuDeYeu(chuDeYeu)
                    .giaiPhap(phanTich.getGiaiPhap())
                    .capNhatLuc(phanTich.getCapNhatLuc())
                    .build();
        } catch (Exception e) {
            throw new RuntimeException("Lỗi khi đọc phân tích", e);
        }
    }

    private String taoGiaiPhap(List<PhanTichHocTapResponse.ChuDeInfo> chuDeYeu, 
                                List<PhanTichHocTapResponse.ChuDeInfo> chuDeManh) {
        StringBuilder sb = new StringBuilder();
        
        if (!chuDeYeu.isEmpty()) {
            sb.append("Các chủ đề cần cải thiện:\n");
            for (PhanTichHocTapResponse.ChuDeInfo chuDe : chuDeYeu) {
                sb.append("- ").append(chuDe.getTen())
                  .append(" (tỉ lệ đúng: ").append(String.format("%.1f", chuDe.getTiLeDung())).append("%)\n");
            }
            sb.append("\nGợi ý: Hãy luyện tập lại các bộ câu hỏi liên quan đến các chủ đề trên.\n");
        }
        
        if (!chuDeManh.isEmpty()) {
            sb.append("\nCác chủ đề bạn đã nắm vững:\n");
            for (PhanTichHocTapResponse.ChuDeInfo chuDe : chuDeManh) {
                sb.append("- ").append(chuDe.getTen())
                  .append(" (tỉ lệ đúng: ").append(String.format("%.1f", chuDe.getTiLeDung())).append("%)\n");
            }
        }
        
        return sb.toString();
    }

    // Helper class để tính toán thống kê theo chủ đề
    private static class ChuDeStats {
        String ten;
        long tongCau = 0;
        long soCauDung = 0;

        ChuDeStats(String ten) {
            this.ten = ten;
        }
    }
}

