package com.app.backend.configurations;


import org.springframework.context.annotation.Configuration;
import org.springframework.messaging.simp.config.MessageBrokerRegistry;
import org.springframework.web.socket.config.annotation.*;

@Configuration
@EnableWebSocketMessageBroker
public class WebSocketConfig implements WebSocketMessageBrokerConfigurer {

    // Cho phép frontend kết nối (có thể thay đổi domain)
    private static final String[] ALLOWED_ORIGINS = {
            "http://localhost:3000",
            "http://localhost:4200",
            "http://127.0.0.1:3000",
            "http://127.0.0.1:4200",
            "http://127.0.0.1:5501",
            "http://localhost:5501"
    };

    @Override
    public void registerStompEndpoints(StompEndpointRegistry registry) {
        registry.addEndpoint("/ws")
                .setAllowedOriginPatterns("*")
                .withSockJS(); // fallback nếu browser không hỗ trợ WS
    }

    @Override
    public void configureMessageBroker(MessageBrokerRegistry registry) {
        // Client gửi message đến server dùng prefix /app (nếu cần @MessageMapping)
        registry.setApplicationDestinationPrefixes("/app");

        // Server broadcast về client qua /topic/**
        registry.enableSimpleBroker("/topic");
        // Nếu cần user-specific: registry.setUserDestinationPrefix("/user");
    }
}
