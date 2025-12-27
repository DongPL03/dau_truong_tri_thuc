package com.app.backend.services.thongke;

import com.app.backend.responses.thongke.CourseStatResponse;

public interface ICourseStatService {
    /**
     * Lấy thống kê chi tiết về courses của user
     * @param userId ID của user
     * @return CourseStatResponse chứa các thống kê về courses
     */
    CourseStatResponse getCourseStatsForUser(Long userId);
}

