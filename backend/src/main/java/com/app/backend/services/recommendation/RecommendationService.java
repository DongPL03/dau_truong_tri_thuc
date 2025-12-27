package com.app.backend.services.recommendation;

import com.app.backend.models.BoCauHoi;
import com.app.backend.models.KhoaHoc;
import com.app.backend.models.KhoaHocBoCauHoi;
import com.app.backend.models.TheGhiNho;
import com.app.backend.models.TienDoKhoaHoc;
import com.app.backend.repositories.IBoCauHoiRepository;
import com.app.backend.repositories.IKhoaHocBoCauHoiRepository;
import com.app.backend.repositories.IKhoaHocRepository;
import com.app.backend.repositories.ITheGhiNhoRepository;
import com.app.backend.repositories.ITienDoKhoaHocRepository;
import com.app.backend.responses.khoahoc.KhoaHoiResponse;
import com.app.backend.responses.recommendation.BoCauHoiRecommendationItemResponse;
import com.app.backend.responses.recommendation.CourseRecommendationItemResponse;
import com.app.backend.responses.recommendation.RecommendationResponse;
import com.app.backend.responses.thongke.TopicStatResponse;
import com.app.backend.services.thongke.IThongKeChuDeService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RecommendationService implements IRecommendationService {

    private final ITheGhiNhoRepository theGhiNhoRepository;
    private final IKhoaHocBoCauHoiRepository khoaHocBoCauHoiRepository;
    private final IKhoaHocRepository khoaHocRepository;
    private final ITienDoKhoaHocRepository tienDoKhoaHocRepository;
    private final IBoCauHoiRepository boCauHoiRepository;
    private final IThongKeChuDeService thongKeChuDeService;

    @Override
    @Transactional(readOnly = true)
    public RecommendationResponse getRecommendationsForUser(Long userId) {
        // 1. Lấy toàn bộ thẻ ghi nhớ của user
        List<TheGhiNho> memos = theGhiNhoRepository.findAllByUserId(userId);
        if (memos.isEmpty()) {
            return RecommendationResponse.builder()
                    .courses(List.of())
                    .boCauHoi(List.of())
                    .build();
        }

        // 2. Gom theo BoCauHoi, đếm số câu distinct sai
        Map<Long, MemoBoStats> memoByBo = new HashMap<>();
        for (TheGhiNho memo : memos) {
            if (memo.getCauHoi() == null || memo.getPhien() == null || memo.getPhien().getBoCauHoi() == null) {
                continue;
            }
            BoCauHoi bo = memo.getPhien().getBoCauHoi();
            Long boId = bo.getId();
            MemoBoStats stats = memoByBo.computeIfAbsent(boId, id -> new MemoBoStats(bo));
            stats.cauHoiIds.add(memo.getCauHoi().getId());
        }

        // 3. Tạo danh sách gợi ý bộ câu hỏi (top 5 theo số câu sai)
        List<BoCauHoiRecommendationItemResponse> boCauHoiRecs = memoByBo.values().stream()
                .sorted(Comparator.comparingInt((MemoBoStats s) -> s.cauHoiIds.size()).reversed())
                .limit(5)
                .map(stats -> {
                    BoCauHoi bo = stats.boCauHoi;
                    int soCauSai = stats.cauHoiIds.size();
                    String chuDeTen = bo.getChuDe() != null ? bo.getChuDe().getTen() : null;
                    String lyDo = "Bạn còn " + soCauSai + " câu hỏi sai trong bộ này";
                    return BoCauHoiRecommendationItemResponse.builder()
                            .boCauHoiId(bo.getId())
                            .tieuDe(bo.getTieuDe())
                            .chuDeTen(chuDeTen)
                            .soCauSai(soCauSai)
                            .lyDo(lyDo)
                            .build();
                })
                .collect(Collectors.toList());

        // 4. Gom theo khóa học dựa trên các bộ có memo
        Map<Long, CourseStats> courseStatsMap = new HashMap<>();
        for (MemoBoStats stats : memoByBo.values()) {
            BoCauHoi bo = stats.boCauHoi;
            Optional<KhoaHocBoCauHoi> optKh = khoaHocBoCauHoiRepository.findByBoCauHoiId(bo.getId());
            if (optKh.isEmpty()) continue; // Bộ câu hỏi không nằm trong khóa học nào

            KhoaHoc khoaHoc = optKh.get().getKhoaHoc();
            if (khoaHoc == null || Boolean.TRUE.equals(khoaHoc.getIsXoa())) continue;

            Long khoaHocId = khoaHoc.getId();
            CourseStats cStats = courseStatsMap.computeIfAbsent(khoaHocId, id -> new CourseStats(khoaHoc));
            cStats.totalMemoQuestions += stats.cauHoiIds.size();
            cStats.boCount++;
        }

        // 5. Gộp thêm thông tin tiến độ: các khóa học đang học dở
        List<TienDoKhoaHoc> activeCourses = tienDoKhoaHocRepository.findActiveKhoaHocByUserId(userId);
        Set<Long> completedCourseIds = new HashSet<>();
        for (TienDoKhoaHoc td : activeCourses) {
            KhoaHoc kh = td.getKhoaHoc();
            if (kh == null || Boolean.TRUE.equals(kh.getIsXoa())) continue;

            CourseStats cStats = courseStatsMap.computeIfAbsent(kh.getId(), id -> new CourseStats(kh));
            cStats.tienDo = td;

            if ("HOAN_THANH".equals(td.getTrangThai())) {
                completedCourseIds.add(kh.getId());
            }
        }

        // 6. Bổ sung gợi ý khóa học từ các chủ đề yếu (ngay cả khi chưa có thẻ ghi nhớ trong khóa học)
        enrichCoursesFromWeakTopics(userId, courseStatsMap, completedCourseIds);

        // 7. Tạo danh sách gợi ý khóa học (ưu tiên: có nhiều câu sai + đang học dở, sau đó theo chủ đề yếu)
        List<CourseRecommendationItemResponse> courseRecs = courseStatsMap.values().stream()
                .sorted(Comparator
                        .comparingInt((CourseStats s) -> s.totalMemoQuestions).reversed()
                        .thenComparing((CourseStats s) -> {
                            if (s.tienDo == null) return 1;
                            // Ưu tiên khóa học đang học dở, chưa hoàn thành
                            return "DANG_HOC".equals(s.tienDo.getTrangThai()) ? 0 : 1;
                        }))
                .limit(5)
                .map(stats -> {
                    KhoaHoc kh = stats.khoaHoc;
                    StringBuilder reason = new StringBuilder();
                    if (stats.totalMemoQuestions > 0) {
                        reason.append("Bạn còn ")
                                .append(stats.totalMemoQuestions)
                                .append(" câu hỏi sai trong các bộ thuộc khóa học này. ");
                    }
                    if (stats.tienDo != null) {
                        if ("DANG_HOC".equals(stats.tienDo.getTrangThai())) {
                            reason.append("Bạn đang học dở khóa học (hoàn thành ~")
                                    .append(stats.tienDo.getPhanTramHoanThanh())
                                    .append("%).");
                        } else if ("HOAN_THANH".equals(stats.tienDo.getTrangThai())) {
                            reason.append("Bạn đã hoàn thành khóa học này, nhưng vẫn còn câu hỏi sai trong các bộ.");
                        }
                    }
                    if (!stats.topicReasons.isEmpty()) {
                        if (reason.length() > 0) {
                            reason.append(" ");
                        }
                        reason.append(String.join(" ", stats.topicReasons));
                    }
                    if (reason.length() == 0) {
                        reason.append("Khóa học liên quan tới các chủ đề bạn đang luyện tập.");
                    }

                    return CourseRecommendationItemResponse.builder()
                            .khoaHoc(KhoaHoiResponse.from(kh))
                            .lyDo(reason.toString())
                            .build();
                })
                .collect(Collectors.toList());

        return RecommendationResponse.builder()
                .courses(courseRecs)
                .boCauHoi(boCauHoiRecs)
                .build();
    }

    /**
     * Bổ sung các khóa học dựa trên chủ đề yếu từ thống kê TopicStatResponse.
     */
    private void enrichCoursesFromWeakTopics(
            Long userId,
            Map<Long, CourseStats> courseStatsMap,
            Set<Long> completedCourseIds
    ) {
        List<TopicStatResponse> stats = thongKeChuDeService.getTopicStatsForUser(userId);
        if (stats == null || stats.isEmpty()) {
            return;
        }

        // Lọc các chủ đề yếu (danhGia = YEU hoặc tỉ lệ đúng < 50%), đủ dữ liệu (tongCau >= 5)
        List<TopicStatResponse> weakTopics = stats.stream()
                .filter(t -> t.getTongCau() != null && t.getTongCau() >= 5)
                .filter(t -> "YEU".equalsIgnoreCase(t.getDanhGia()) || (t.getTiLeDung() != null && t.getTiLeDung() < 50.0))
                .limit(5)
                .collect(Collectors.toList());

        if (weakTopics.isEmpty()) {
            return;
        }

        for (TopicStatResponse t : weakTopics) {
            Long chuDeId = t.getChuDeId();
            if (chuDeId == null) continue;

            // Lấy một số khóa học published của chủ đề này
            var page = khoaHocRepository.findPublishedKhoaHoc(
                    org.springframework.data.domain.PageRequest.of(0, 10),
                    chuDeId
            );

            page.getContent().forEach(kh -> {
                if (Boolean.TRUE.equals(kh.getIsXoa())) return;
                Long khId = kh.getId();
                if (completedCourseIds.contains(khId)) {
                    // Bỏ qua các khóa học đã hoàn thành hẳn
                    return;
                }

                CourseStats cStats = courseStatsMap.computeIfAbsent(khId, id -> new CourseStats(kh));
                String reason = "Bạn còn yếu chủ đề \"" + t.getChuDeTen()
                        + "\" (tỉ lệ đúng " + String.format("%.1f", t.getTiLeDung()) + "%). "
                        + "Khóa học này thuộc chủ đề đó, phù hợp để cải thiện.";
                cStats.topicReasons.add(reason);
            });
        }
    }

    private static class MemoBoStats {
        final BoCauHoi boCauHoi;
        final Set<Long> cauHoiIds = new HashSet<>();

        MemoBoStats(BoCauHoi boCauHoi) {
            this.boCauHoi = boCauHoi;
        }
    }

    private static class CourseStats {
        final KhoaHoc khoaHoc;
        int totalMemoQuestions = 0;
        int boCount = 0;
        TienDoKhoaHoc tienDo;
        List<String> topicReasons = new ArrayList<>();

        CourseStats(KhoaHoc khoaHoc) {
            this.khoaHoc = khoaHoc;
        }
    }
}


