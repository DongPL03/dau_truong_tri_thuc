package com.app.backend.components;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

@Component("customCorsFilter")
@Order(Ordered.HIGHEST_PRECEDENCE)
public class CorsFilter extends OncePerRequestFilter {
    /**
     * Comma-separated allowlist, ví dụ:
     * app.cors.allowed-origins=http://localhost:4200,http://127.0.0.1:4200
     */
    @Value("${app.cors.allowed-origins:http://localhost:4200}")
    private String allowedOrigins;

    private boolean isAllowedOrigin(String origin) {
        if (origin == null || origin.isBlank()) return false;
        Set<String> allow = Arrays.stream(allowedOrigins.split(","))
                .map(String::trim)
                .filter(s -> !s.isBlank())
                .collect(Collectors.toSet());
        return allow.contains(origin);
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {

        String origin = request.getHeader("Origin");

        // 🔒 Chỉ cho phép các origin trong allowlist (tránh phản chiếu Origin mở rộng quá mức)
        if (isAllowedOrigin(origin)) {
            response.setHeader("Access-Control-Allow-Origin", origin);
        }

        // 🔹 Cho phép credentials
        response.setHeader("Access-Control-Allow-Credentials", "true");

        response.setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
        response.setHeader("Access-Control-Max-Age", "3600");
        response.setHeader("Access-Control-Allow-Headers", "authorization, content-type, xsrf-token, accept-language");
        response.addHeader("Access-Control-Expose-Headers", "xsrf-token");

        if ("OPTIONS".equalsIgnoreCase(request.getMethod())) {
            response.setStatus(HttpServletResponse.SC_OK);
        } else {
            filterChain.doFilter(request, response);
        }
    }
}
