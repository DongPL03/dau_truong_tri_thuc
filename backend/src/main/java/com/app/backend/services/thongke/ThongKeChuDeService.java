package com.app.backend.services.thongke;


import com.app.backend.repositories.ITraLoiTranDauRepository;
import com.app.backend.responses.thongke.TopicStatResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;

@Service
@RequiredArgsConstructor
public class ThongKeChuDeService implements IThongKeChuDeService {

    private final ITraLoiTranDauRepository traLoiTranDauRepository;

    @Override
    public List<TopicStatResponse> getTopicStatsForUser(Long userId) {
        // 1. Lấy dữ liệu (DTO đã tự tính toán logic rate/rank bên trong)
        List<TopicStatResponse> stats = traLoiTranDauRepository.getTopicStatsRawByUser(userId);

        if (stats.isEmpty()) {
            return stats;
        }

        // 2. Sắp xếp: MANH -> TRUNG_BINH -> YEU, sau đó giảm dần theo tỉ lệ đúng
        // Dùng Stream hoặc List.sort đều được, ở đây dùng List.sort cho tối ưu bộ nhớ
        stats.sort(
                Comparator.comparingInt((TopicStatResponse r) -> getRankPriority(r.getDanhGia())) // Sort theo nhóm
                        .thenComparing(TopicStatResponse::getTiLeDung, Comparator.reverseOrder()) // Sort theo điểm giảm dần
        );

        return stats;
    }

    // Helper method để định nghĩa thứ tự ưu tiên
    private int getRankPriority(String danhGia) {
        return switch (danhGia) {
            case "MANH" -> 1;
            case "TRUNG_BINH" -> 2;
            case "YEU" -> 3;
            default -> 4;
        };
    }
}
