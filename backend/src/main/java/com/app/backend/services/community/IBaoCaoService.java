package com.app.backend.services.community;

import com.app.backend.dtos.community.BaoCaoDTO;
import com.app.backend.dtos.community.XuLyBaoCaoDTO;
import com.app.backend.responses.community.BaoCaoResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.Map;

public interface IBaoCaoService {

    /**
     * Tạo báo cáo vi phạm
     */
    BaoCaoResponse createReport(Long userId, BaoCaoDTO dto) throws Exception;

    /**
     * Lấy báo cáo của user
     */
    Page<BaoCaoResponse> getMyReports(Long userId, Pageable pageable);

    /**
     * Lấy tất cả báo cáo (Admin)
     */
    Page<BaoCaoResponse> getAllReports(String status, Pageable pageable);

    /**
     * Lấy báo cáo đang chờ xử lý (Admin)
     */
    Page<BaoCaoResponse> getPendingReports(Pageable pageable);

    /**
     * Lấy chi tiết báo cáo (Admin)
     */
    BaoCaoResponse getReportById(Long reportId) throws Exception;

    /**
     * Xử lý báo cáo (Admin)
     */
    BaoCaoResponse resolveReport(Long adminId, Long reportId, XuLyBaoCaoDTO dto) throws Exception;

    /**
     * Bỏ qua báo cáo (Admin)
     */
    BaoCaoResponse dismissReport(Long adminId, Long reportId) throws Exception;

    /**
     * Đếm số báo cáo pending
     */
    long countPendingReports();

    /**
     * Thống kê báo cáo
     */
    Map<String, Object> getReportStats();
}
